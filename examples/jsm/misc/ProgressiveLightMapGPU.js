import { BackSide, Box3, BufferAttribute, Color, DoubleSide, FloatType, HalfFloatType, LinearFilter, NearestFilter, ArrayCamera, OrthographicCamera, Matrix4, Quaternion, Sphere, Vector2, Vector3, Vector4, PlaneGeometry, Mesh, RenderTarget, Scene, MeshBasicNodeMaterial, MeshPhongNodeMaterial, NodeMaterial } from 'three/webgpu';
import { any, attribute, cameraIndex, dot, equirectUV, float, Fn, hash, If, int, length, Loop, max, mix, normalWorld, output, positionLocal, positionWorld, screenCoordinate, select, sub, texture, uniform, uniformArray, uv, vec2, vec3, vec4 } from 'three/tsl';

import { potpack } from '../libs/potpack.module.js';

const _sphere = /*@__PURE__*/ new Sphere();
const _box = /*@__PURE__*/ new Box3();
const _quaternion = /*@__PURE__*/ new Quaternion();
const _corner = /*@__PURE__*/ new Vector3();
const _target = /*@__PURE__*/ new Vector3();

/**
 * Progressive Light Map Accumulator, by [zalo](https://github.com/zalo/).
 *
 * To use, simply construct a `ProgressiveLightMap` object,
 * `plmap.addObjectsToLightMap(object)` an array of semi-static
 * objects and lights to the class once, and then call
 * `plmap.update(camera)` (direct lighting) and/or `plmap.updateBounce(camera)`
 * (indirect radiosity) every frame to begin accumulating lighting samples.
 *
 * The indirect radiosity solver is a clean re-implementation of
 * *"Approximate Radiosity using Stochastic Depth Buffering"*
 * (Thomsen & Nielsen, 2010). Instead of gathering one global direction per
 * frame, it uses an {@link ArrayCamera} to rasterize `directions` global
 * directions simultaneously (each into its own viewport tile of a single
 * "illumination map"), then gathers all of them in one pass. Visibility along
 * each direction is approximated with *stochastic depth buffering*: a random
 * per-fragment depth (written via `NodeMaterial.depthNode`) shuffles
 * overlapping surfaces so a small texel neighborhood statistically represents
 * the set of surfaces along a ray.
 *
 * Energy is integrated with a principled progressive (cumulative moving)
 * average rather than an ad-hoc leaky blend, so the lightmap converges to the
 * true radiosity solution. The diffuse bounce is PBR-correct: gathered
 * radiance is modulated by the *receiving* surface's albedo and an emissive
 * term seeds the solution, i.e. `Lo = Le + 4·ρ·(1/N)·Σ Lk·max(dot(n,dk),0)`.
 *
 * This class can only be used with {@link WebGPURenderer}.
 * When using {@link WebGLRenderer}, import from `ProgressiveLightMap.js`.
 *
 * @three_import import { ProgressiveLightMap } from 'three/addons/misc/ProgressiveLightMapGPU.js';
 */
class ProgressiveLightMap {

	/**
	 * @param {WebGPURenderer} renderer - The renderer.
	 * @param {number} [resolution=1024] - The side-long dimension of the total lightmap.
	 * @param {Object} [options={}] - Indirect-radiosity options.
	 * @param {boolean} [options.radiosity=false] - Enables the indirect radiosity solver and `updateBounce()`.
	 * @param {number} [options.directions=16] - Number of global directions rasterized simultaneously per frame.
	 * @param {number} [options.tileResolution=256] - Per-direction illumination-map tile resolution (pixels).
	 * @param {number} [options.gatherRadius=1] - Neighborhood half-width: `1` → 3×3, `2` → 5×5 gather.
	 * @param {number} [options.maxHistory=0] - Caps the progressive integrator. `0` is a converging cumulative
	 * mean (sharpest, but freezes residual noise); a finite value `N` is leaky/exponential blending with window
	 * `N` (the original scheme — keeps refreshing, adapts to dynamic scenes).
	 * @param {number} [options.maxSamples=0] - Accumulated directions (bounces × directions) after which
	 * {@link ProgressiveLightMap#updateBounce} stops refining; `0` never stops. Counting directions rather than
	 * bounces makes a 4×4 ArrayCamera reach the budget four times faster than 1×1.
	 * @param {string} [options.envSampling='importance'] - How the global directions are drawn: `'uniform'`
	 * (rotating Fibonacci sphere) or `'importance'` (half uniform, half proportional to the environment's
	 * luminance from {@link ProgressiveLightMap#setEnvironmentImportance}, combined with the balance heuristic).
	 * @param {number} [options.fadeInSamples=0] - Accumulated directions over which the displayed lightmap
	 * fades in (smoothstep), hiding the noisy first bounces; `0` disables the fade.
	 */
	constructor( renderer, resolution = 1024, options = {} ) {

		/**
		 * The renderer.
		 *
		 * @type {WebGPURenderer}
		 */
		this.renderer = renderer;

		/**
		 * The side-long dimension of the total lightmap.
		 *
		 * @type {number}
		 * @default 1024
		 */
		this.resolution = resolution;

		this._lightMapContainers = [];
		this._scene = new Scene();
		this._buffer1Active = false;
		this._labelMesh = null;
		this._illumDebugMesh = null;
		this._blurringPlane = null;

		// Create the Progressive LightMap Texture

		const type = /(Android|iPad|iPhone|iPod)/g.test( navigator.userAgent ) ? HalfFloatType : FloatType;
		this._progressiveLightMap1 = new RenderTarget( this.resolution, this.resolution, { type: type } );
		this._progressiveLightMap2 = new RenderTarget( this.resolution, this.resolution, { type: type } );
		this._progressiveLightMap2.texture.channel = 1;

		// uniforms

		this._averagingWindow = uniform( 100 );
		this._previousShadowMap = texture( this._progressiveLightMap1.texture );

		// materials

		const uvNode = uv( 1 ).flipY();

		this._uvMat = new MeshPhongNodeMaterial();
		this._uvMat.vertexNode = vec4( sub( uvNode, vec2( 0.5 ) ).mul( 2 ), 1, 1 );
		this._uvMat.outputNode = vec4( mix( this._previousShadowMap.sample( uv( 1 ) ), output, float( 1 ).div( this._averagingWindow ) ) );

		// Indirect radiosity (Stochastic Depth Buffering)

		this.radiosity = options.radiosity === true;

		if ( this.radiosity === true ) {

			this._initializeRadiosity( options );

		}

	}

	/**
	 * Sets these objects' materials' lightmaps and modifies their uv1's.
	 *
	 * @param {Array<Object3D>} objects - An array of objects and lights to set up your lightmap.
	 */
	addObjectsToLightMap( objects ) {

		// Prepare list of UV bounding boxes for packing later...
		const uv_boxes = [];

		const padding = 3 / this.resolution;

		for ( let ob = 0; ob < objects.length; ob ++ ) {

			const object = objects[ ob ];

			// If this object is a light, simply add it to the internal scene
			if ( object.isLight ) {

				this._scene.attach( object ); continue;

			}

			if ( object.geometry.hasAttribute( 'uv' ) === false ) {

				console.warn( 'THREE.ProgressiveLightMap: All lightmap objects need uvs.' ); continue;

			}

			if ( object.geometry.hasAttribute( 'normal' ) === false ) {

				console.warn( 'THREE.ProgressiveLightMap: All lightmap objects need normals.' ); continue;

			}

			if ( this._blurringPlane === null ) {

				this._initializeBlurPlane();

			}

			// Apply the lightmap to the object
			object.material.lightMap = this._progressiveLightMap2.texture;
			object.material.dithering = true;
			object.castShadow = true;
			object.receiveShadow = true;
			object.renderOrder = 1000 + ob;

			// Prepare UV boxes for potpack (potpack will update x and y)
			// TODO: Size these by object surface area
			uv_boxes.push( { w: 1 + ( padding * 2 ), h: 1 + ( padding * 2 ), index: ob, x: 0, y: 0 } );

			const container = { basicMat: object.material, object: object };

			if ( this.radiosity === true ) {

				this._initializeRadiosityObject( container );

			}

			this._lightMapContainers.push( container );

		}

		// Pack the objects' lightmap UVs into the same global space
		const dimensions = potpack( uv_boxes );
		uv_boxes.forEach( ( box ) => {

			const uv1 = objects[ box.index ].geometry.getAttribute( 'uv' ).clone();
			for ( let i = 0; i < uv1.array.length; i += uv1.itemSize ) {

				uv1.array[ i ] = ( uv1.array[ i ] + box.x + padding ) / dimensions.w;
				uv1.array[ i + 1 ] = 1 - ( ( uv1.array[ i + 1 ] + box.y + padding ) / dimensions.h );

			}

			objects[ box.index ].geometry.setAttribute( 'uv1', uv1 );
			objects[ box.index ].geometry.getAttribute( 'uv1' ).needsUpdate = true;

		} );

		if ( this.radiosity === true ) {

			this._computeBounds();

		}

	}

	/**
	 * Frees all internal resources.
	 */
	dispose() {

		this._progressiveLightMap1.dispose();
		this._progressiveLightMap2.dispose();

		this._uvMat.dispose();

		if ( this._blurringPlane !== null ) {

			this._blurringPlane.geometry.dispose();
			this._blurringPlane.material.dispose();

		}

		if ( this._labelMesh !== null ) {

			this._labelMesh.geometry.dispose();
			this._labelMesh.material.dispose();

		}

		if ( this._illumDebugMesh !== null ) {

			this._illumDebugMesh.geometry.dispose();
			this._illumDebugMesh.material.dispose();

		}

		if ( this.radiosity === true ) {

			this._radiosityMap1.dispose();
			this._radiosityMap2.dispose();
			this._illuminationMap.dispose();
			this._illuminationMaterial.dispose();
			this._gatherMaterial.dispose();
			this._displayMaterial.dispose();
			if ( this._environmentMaterial !== null ) this._environmentMaterial.dispose();
			if ( this._environmentTableMaterial !== null ) this._environmentTableMaterial.dispose();
			if ( this._environmentTableRT !== null ) this._environmentTableRT.dispose();

		}

	}

	/**
	 * This function renders each mesh one at a time into their respective surface maps.
	 *
	 * @param {Camera} camera - The camera the scene is rendered with.
	 * @param {number} [blendWindow=100] - When >1, samples will accumulate over time.
	 * @param {boolean} [blurEdges=true] - Whether to fix UV Edges via blurring.
	 */
	update( camera, blendWindow = 100, blurEdges = true ) {

		if ( this._blurringPlane === null ) {

			return;

		}

		// Store the original Render Target
		const currentRenderTarget = this.renderer.getRenderTarget();

		// The blurring plane applies blur to the seams of the lightmap
		this._blurringPlane.visible = blurEdges;

		// Steal the Object3D from the real world to our special dimension
		for ( let l = 0; l < this._lightMapContainers.length; l ++ ) {

			this._lightMapContainers[ l ].object.oldScene = this._lightMapContainers[ l ].object.parent;
			this._scene.attach( this._lightMapContainers[ l ].object );

		}

		// Set each object's material to the UV Unwrapped Surface Mapping Version
		for ( let l = 0; l < this._lightMapContainers.length; l ++ ) {

			this._averagingWindow.value = blendWindow;
			this._lightMapContainers[ l ].object.material = this._uvMat;
			this._lightMapContainers[ l ].object.oldFrustumCulled = this._lightMapContainers[ l ].object.frustumCulled;
			this._lightMapContainers[ l ].object.frustumCulled = false;

		}

		// Ping-pong two surface buffers for reading/writing
		const activeMap = this._buffer1Active ? this._progressiveLightMap1 : this._progressiveLightMap2;
		const inactiveMap = this._buffer1Active ? this._progressiveLightMap2 : this._progressiveLightMap1;

		// Render the object's surface maps
		this.renderer.setRenderTarget( activeMap );
		this._previousShadowMap.value = inactiveMap.texture;

		this._buffer1Active = ! this._buffer1Active;
		this.renderer.render( this._scene, camera );

		// Restore the object's Real-time Material and add it back to the original world
		for ( let l = 0; l < this._lightMapContainers.length; l ++ ) {

			this._lightMapContainers[ l ].object.frustumCulled = this._lightMapContainers[ l ].object.oldFrustumCulled;
			this._lightMapContainers[ l ].object.material = this._lightMapContainers[ l ].basicMat;
			this._lightMapContainers[ l ].object.oldScene.attach( this._lightMapContainers[ l ].object );

		}

		// Restore the original Render Target
		this.renderer.setRenderTarget( currentRenderTarget );

	}

	/**
	 * Accumulates one progressive sample of the indirect (radiosity) bounce.
	 *
	 * Each call rasterizes `directions` global directions at once with the
	 * internal {@link ArrayCamera} (stochastic-depth illumination map), gathers
	 * the incident radiance for every lightmap texel, modulates it by the
	 * receiving surface's albedo, adds emission, and folds the result into the
	 * radiosity lightmap with a cumulative moving average.
	 *
	 * Requires `radiosity: true` at construction. The resulting radiosity is
	 * exposed via {@link ProgressiveLightMap#lightMapTexture} and is shown by the
	 * display materials assigned to the lightmapped objects.
	 *
	 * @param {Camera} camera - The camera the scene is rendered with.
	 * @param {boolean} [blurEdges=true] - Whether to fix UV Edges via blurring.
	 */
	updateBounce( camera, blurEdges = true ) {

		if ( this.radiosity !== true || this._blurringPlane === null || this.converged ) {

			return;

		}

		const renderer = this.renderer;

		// Store renderer state we are about to override
		const currentRenderTarget = renderer.getRenderTarget();

		if ( this._needsClear ) this._clearRadiosityMaps();
		this._advanceEstimator();
		const env = this._stealObjects();

		const readRT = this._buffer1Active ? this._radiosityMap1 : this._radiosityMap2;
		const writeRT = this._buffer1Active ? this._radiosityMap2 : this._radiosityMap1;

		this._illuminationPass( readRT );
		this._gatherPass( camera, readRT, writeRT, env, blurEdges );

		this._restoreObjects( env );

		renderer.setRenderTarget( currentRenderTarget );

	}

	/**
	 * Like {@link ProgressiveLightMap#updateBounce}, but measures the GPU time of
	 * the two heavy passes separately and resolves with the per-pass timings (in
	 * milliseconds). Each pass is followed by `queue.onSubmittedWorkDone()` so the
	 * timing reflects GPU execution, not just CPU command submission. This forces
	 * a CPU↔GPU sync per pass (slow), so use it for diagnostics only — not every
	 * frame. Works on any WebGPU backend (no `TimestampQuery` feature required, so
	 * it's reliable on mobile, where that feature is often unavailable).
	 *
	 * To avoid interleaving with the render loop, pause it (`setAnimationLoop( null )`)
	 * around the call, and discard the first result (pipeline warm-up).
	 *
	 * @async
	 * @param {Camera} camera - The camera the scene is rendered with.
	 * @param {boolean} [blurEdges=true] - Whether to fix UV Edges via blurring.
	 * @return {Promise<?{illumination: number, gather: number, total: number}>} The per-pass GPU times (ms), or `null` if unavailable.
	 */
	async profileBounce( camera, blurEdges = true ) {

		if ( this.radiosity !== true || this._blurringPlane === null ) return null;

		const renderer = this.renderer;

		// Reliable cross-backend GPU fence: the WebGPU queue's onSubmittedWorkDone.
		// Falls back to null on non-WebGPU backends (e.g. a WebGL fallback), where
		// there is no such fence — the caller then reports "unavailable".
		const device = ( renderer.backend && renderer.backend.device ) || null;
		const queue = device ? device.queue : null;
		if ( ! queue || typeof queue.onSubmittedWorkDone !== 'function' ) return null;

		// Race the fence against a watchdog so a wedged/lost device can never hang
		// the profiler forever (which would leave the render loop paused).
		const sync = () => Promise.race( [
			queue.onSubmittedWorkDone(),
			new Promise( ( resolve, reject ) => setTimeout( () => reject( new Error( 'GPU sync timed out' ) ), 10000 ) )
		] );

		const currentRenderTarget = renderer.getRenderTarget();

		this._advanceEstimator();
		const env = this._stealObjects();

		// try/finally so the scene is ALWAYS restored — even if a sync rejects —
		// otherwise the stolen objects/materials would be left swapped in.
		try {

			const readRT = this._buffer1Active ? this._radiosityMap1 : this._radiosityMap2;
			const writeRT = this._buffer1Active ? this._radiosityMap2 : this._radiosityMap1;

			await sync(); // drain prior work so we time only our passes

			const t0 = performance.now();
			this._illuminationPass( readRT );
			await sync();
			const t1 = performance.now();

			this._gatherPass( camera, readRT, writeRT, env, blurEdges );
			await sync();
			const t2 = performance.now();

			return { illumination: t1 - t0, gather: t2 - t1, total: t2 - t0 };

		} finally {

			this._restoreObjects( env );
			renderer.setRenderTarget( currentRenderTarget );

		}

	}

	/**
	 * Advances the progressive estimator's sample count and refreshes the global
	 * directions / ArrayCamera tiles for the current frame.
	 *
	 * @private
	 */
	_advanceEstimator() {

		this._frameCount ++;
		this._frameSeed.value = this._frameCount;
		const n = this._maxHistory > 0 ? Math.min( this._frameCount, this._maxHistory ) : this._frameCount;
		this._invSampleCount.value = 1 / n;

		if ( this._fadeInSamples > 0 ) {

			const t = Math.min( 1, this.samples / this._fadeInSamples );
			this._fadeWeight.value = t * t * ( 3 - 2 * t );

		}

		// Refresh the global directions and their ArrayCamera tiles
		this._updateDirections();

	}

	/**
	 * Steals the lightmapped objects (and, if enabled, the environment) into the
	 * private radiosity scene for the illumination/gather passes.
	 *
	 * @private
	 * @return {?Object3D} The stolen environment object, or `null`.
	 */
	_stealObjects() {

		// Steal the lightmapped objects into our private scene
		for ( const container of this._lightMapContainers ) {

			container.object.oldScene = container.object.parent;
			container.object.oldFrustumCulled = container.object.frustumCulled;
			container.object.frustumCulled = false;
			this._scene.attach( container.object );

		}

		// Steal the environment (sky) too, swapping in its illumination material
		const env = ( this._environment !== null && this._environmentEnabled ) ? this._environment : null;
		if ( env !== null ) {

			env.oldScene = env.parent;
			env.oldMaterial = env.material;
			env.oldFrustumCulled = env.frustumCulled;
			env.frustumCulled = false;
			env.material = this._environmentMaterial;
			this._scene.attach( env );

		}

		return env;

	}

	/**
	 * Restores the lightmapped objects' display material / original parent and the
	 * environment after a bounce.
	 *
	 * @private
	 * @param {?Object3D} env - The environment object returned by {@link ProgressiveLightMap#_stealObjects}.
	 */
	_restoreObjects( env ) {

		for ( const container of this._lightMapContainers ) {

			container.object.frustumCulled = container.object.oldFrustumCulled;
			container.object.material = this._displayMaterial;
			container.object.oldScene.attach( container.object );

		}

		if ( env !== null ) {

			env.visible = true;
			env.frustumCulled = env.oldFrustumCulled;
			env.material = env.oldMaterial;
			env.oldScene.attach( env );

		}

		this._blurringPlane.visible = false;

	}

	/**
	 * ILLUMINATION PASS — rasterize N global directions into tiles of one map.
	 * Each surface stores its current outgoing radiance (rgb) and its signed
	 * distance along the tile's direction (a), with a random fragment depth.
	 *
	 * @private
	 * @param {RenderTarget} readRT - The radiosity map sampled as the radiance source.
	 */
	_illuminationPass( readRT ) {

		const renderer = this.renderer;
		const currentPixelRatio = renderer.getPixelRatio();
		const currentClearAlpha = renderer.getClearAlpha();
		renderer.getClearColor( this._clearColor );

		this._lightMapSource.value = readRT.texture;
		this._blurringPlane.visible = false;

		for ( const container of this._lightMapContainers ) {

			container.object.material = this._illuminationMaterial;

		}

		renderer.setPixelRatio( 1 ); // tile viewports are in render-target texels (see WebGPUBackend viewport*pixelRatio)
		renderer.setClearColor( 0x000000, 0 ); // alpha 0 marks "no surface" texels (rejected during gather)
		renderer.setRenderTarget( this._illuminationMap );
		renderer.clear();
		renderer.render( this._scene, this._arrayCamera );
		renderer.setPixelRatio( currentPixelRatio );
		renderer.setClearColor( this._clearColor, currentClearAlpha );

	}

	/**
	 * GATHER PASS — for every lightmap texel, gather the nearest in-hemisphere
	 * surface along each direction, weight by cos and albedo, add emission, and
	 * fold into the radiosity map with the progressive mean.
	 *
	 * @private
	 * @param {Camera} camera - The camera the scene is rendered with.
	 * @param {RenderTarget} readRT - The previous radiosity map (estimator history + seam-dilation source).
	 * @param {RenderTarget} writeRT - The radiosity map to write the new sample into.
	 * @param {?Object3D} env - The stolen environment object, or `null`.
	 * @param {boolean} blurEdges - Whether to fix UV Edges via blurring.
	 */
	_gatherPass( camera, readRT, writeRT, env, blurEdges ) {

		const renderer = this.renderer;

		this._prevRadiosity.value = readRT.texture;
		this._previousShadowMap.value = readRT.texture; // seam-dilation source for the blurring plane
		this._blurringPlane.visible = blurEdges;

		for ( const container of this._lightMapContainers ) {

			container.object.material = this._gatherMaterial;

		}

		// The environment is a source only — keep it out of the (uv-unwrapped) gather.
		if ( env !== null ) env.visible = false;

		renderer.setRenderTarget( writeRT );
		renderer.render( this._scene, camera );

		this._buffer1Active = ! this._buffer1Active;

		// Expose the freshly written radiosity to the display material
		this._lightMapSource.value = writeRT.texture;

	}

	/**
	 * Resets the progressive radiosity accumulation. Call this whenever the
	 * scene, materials or lighting change so the estimator restarts cleanly.
	 */
	reset() {

		this._frameCount = 0;
		this._needsClear = true;
		if ( this._fadeWeight !== undefined ) this._fadeWeight.value = this._fadeInSamples > 0 ? 0 : 1;

	}

	/**
	 * Accumulated directions so far (bounces × directions per bounce).
	 *
	 * @type {number}
	 */
	get samples() {

		return this._frameCount * ( this._directions || 0 );

	}

	/**
	 * Whether the sample budget (`maxSamples`) has been spent; never when the budget is `0`.
	 *
	 * @type {boolean}
	 */
	get converged() {

		return this._maxSamples > 0 && this.samples >= this._maxSamples;

	}

	/**
	 * Sets the sample budget in accumulated directions (`0` = unlimited).
	 *
	 * @param {number} count - Accumulated directions after which refinement stops.
	 */
	setMaxSamples( count ) {

		this._maxSamples = Math.max( 0, Math.floor( count ) );

	}

	/**
	 * Sets the number of accumulated directions over which the displayed lightmap fades in (`0` = no fade).
	 *
	 * @param {number} count - Accumulated directions.
	 */
	setFadeIn( count ) {

		this._fadeInSamples = Math.max( 0, Math.floor( count ) );
		if ( this._fadeInSamples === 0 ) this._fadeWeight.value = 1;

	}

	/**
	 * Switches between uniform and environment importance sampling of the global
	 * directions. Importance sampling needs a luminance table from
	 * {@link ProgressiveLightMap#setEnvironmentImportance}; without one it behaves like uniform.
	 *
	 * @param {string} mode - `'uniform'` or `'importance'`.
	 */
	setEnvSampling( mode ) {

		if ( this.radiosity !== true ) return;

		const flag = mode !== 'uniform';
		if ( flag === this._importance ) return;

		this._importance = flag;
		this._rebuildBaseDirections();
		this.reset();

	}

	/**
	 * Overrides the importance-sampling distribution with one built on the CPU
	 * from an equirectangular texture (same mapping as TSL's `equirectUV`).
	 * Normally not needed: {@link ProgressiveLightMap#addEnvironment} builds the
	 * table from the radiance node itself. Pass `null` to drop the table (uniform sampling).
	 *
	 * @param {?Texture} texture - Equirectangular environment texture (image or data texture).
	 */
	setEnvironmentImportance( texture ) {

		if ( this.radiosity !== true ) return;

		this._importanceTable = texture ? EnvImportanceTable.fromTexture( texture ) : null;
		this._rebuildBaseDirections();
		this.reset();

	}

	/**
	 * Zeroes both radiosity maps. Render targets start with undefined contents
	 * (often NaN on real GPUs), and the first bounce samples the previous map
	 * for the bounce radiance and the seam blur.
	 *
	 * @private
	 */
	_clearRadiosityMaps() {

		const renderer = this.renderer;
		const currentRenderTarget = renderer.getRenderTarget();
		const currentClearAlpha = renderer.getClearAlpha();
		renderer.getClearColor( this._clearColor );
		renderer.setClearColor( 0x000000, 0 );

		for ( const rt of [ this._radiosityMap1, this._radiosityMap2 ] ) {

			renderer.setRenderTarget( rt );
			renderer.clear();

		}

		renderer.setRenderTarget( currentRenderTarget );
		renderer.setClearColor( this._clearColor, currentClearAlpha );
		this._needsClear = false;

	}

	/**
	 * Replaces NaN/Inf/negative texels by zero so one bad texel cannot poison
	 * the progressive mean or spread through the seam blur.
	 *
	 * @private
	 * @param {Node<vec4>} c - The texel.
	 * @return {Node<vec4>} The sanitized texel.
	 */
	_saneNode( c ) {

		return select( any( c.notEqual( c ) ), vec4( 0 ), max( c, vec4( 0 ) ) );

	}

	/**
	 * Directions per bounce drawn uniformly; the rest come from the environment table.
	 *
	 * @private
	 * @return {number} The uniform count.
	 */
	_uniformCount() {

		const useTable = this._importance && this._importanceTable !== null && this._environment !== null && this._environmentEnabled;
		if ( ! useTable ) return this._directions;
		if ( this._directions === 1 ) return this._frameCount % 2 === 0 ? 1 : 0;
		return Math.ceil( this._directions / 2 );

	}

	/**
	 * Rebuilds the Fibonacci set used for the uniformly drawn directions.
	 *
	 * @private
	 */
	_rebuildBaseDirections() {

		const n = this._importance && this._importanceTable !== null
			? Math.max( 1, Math.ceil( this._directions / 2 ) )
			: this._directions;

		this._baseDirections = [];
		for ( let k = 0; k < n; k ++ ) this._baseDirections.push( this._fibonacciSphere( k, n ) );

	}

	/**
	 * Sets the gather neighborhood half-width (`1` → 3×3, `2` → 5×5) and rebuilds
	 * the shared gather material. Larger kernels capture more hidden surfaces per
	 * direction at the cost of more texture reads.
	 *
	 * @param {number} radius - The neighborhood half-width.
	 */
	setGatherRadius( radius ) {

		if ( this.radiosity !== true ) return;

		this._gatherRadius = Math.max( 1, Math.floor( radius ) );

		const oldMat = this._gatherMaterial;
		this._gatherMaterial = this._createGatherMaterial( this._debugMode );
		if ( oldMat ) oldMat.dispose();

		this.reset();

	}

	/**
	 * Sets the number of global directions rasterized per frame — i.e. the
	 * ArrayCamera tile grid. Use a perfect square for a square grid: `1` (1×1),
	 * `4` (2×2) or `16` (4×4). Fewer directions make the gather dramatically
	 * cheaper (its per-direction loop is host-unrolled at this count, so the count
	 * is baked into the shader) but converge slower / noisier per frame.
	 *
	 * Rebuilds all direction-dependent state: the tile layout, the illumination-map
	 * render target, the sub-cameras / ArrayCamera, the per-direction uniform arrays
	 * and the illumination + gather materials.
	 *
	 * @param {number} count - The number of directions (ArrayCamera tiles).
	 */
	setDirectionCount( count ) {

		if ( this.radiosity !== true ) return;

		count = Math.max( 1, Math.floor( count ) );
		if ( count === this._directions ) return;

		this._directions = count;

		// Recompute the tile layout and resize the illumination map
		this._tilesX = Math.ceil( Math.sqrt( this._directions ) );
		this._tilesY = Math.ceil( this._directions / this._tilesX );
		const illumW = this._tilesX * this._tileResolution;
		const illumH = this._tilesY * this._tileResolution;

		this._illuminationMap.dispose();
		this._illuminationMap = new RenderTarget( illumW, illumH, { type: FloatType, minFilter: NearestFilter, magFilter: NearestFilter, generateMipmaps: false, depthBuffer: true } );
		this._illuminationTex.value = this._illuminationMap.texture; // node identity kept; only the value swaps

		// Rebuild the per-direction CPU state and sub-cameras
		this._scene.remove( this._arrayCamera );

		this._directionData = [];
		this._basisUData = [];
		this._basisVData = [];
		this._viewProjData = [];
		this._tileOffsetData = [];
		this._dirWeightData = [];
		this._originData = [];
		this._halfData = [];
		this._baseDirections = [];
		this._subCameras = [];

		const half = this._tileResolution * 0.5;

		for ( let k = 0; k < this._directions; k ++ ) {

			this._directionData.push( new Vector3( 0, 1, 0 ) );
			this._basisUData.push( new Vector3( 1, 0, 0 ) );
			this._basisVData.push( new Vector3( 0, 0, 1 ) );
			this._viewProjData.push( new Matrix4() );

			const tx = k % this._tilesX;
			const ty = Math.floor( k / this._tilesX );
			this._tileOffsetData.push( new Vector2( tx / this._tilesX, ty / this._tilesY ) );

			this._dirWeightData.push( 4 / this._directions );
			this._originData.push( new Vector3() );
			this._halfData.push( new Vector2( 1, 1 ) );

			const subCamera = new OrthographicCamera( - half, half, half, - half, 0, 1 );
			subCamera.viewport = new Vector4( tx * this._tileResolution, ty * this._tileResolution, this._tileResolution, this._tileResolution );
			this._subCameras.push( subCamera );

		}

		this._rebuildBaseDirections();
		this._arrayCamera = new ArrayCamera( this._subCameras );
		this._scene.add( this._arrayCamera );

		// Re-create the fixed-length uniform arrays (their size is baked) and refresh
		// the layout-dependent scalar uniforms.
		this._directionArray = uniformArray( this._directionData, 'vec3' );
		this._basisUArray = uniformArray( this._basisUData, 'vec3' );
		this._basisVArray = uniformArray( this._basisVData, 'vec3' );
		this._viewProjArray = uniformArray( this._viewProjData );
		this._tileOffsetArray = uniformArray( this._tileOffsetData, 'vec2' );
		this._dirWeightArray = uniformArray( this._dirWeightData, 'float' );
		this._originArray = uniformArray( this._originData, 'vec3' );
		this._halfArray = uniformArray( this._halfData, 'vec2' );

		this._tileScale.value.set( 1 / this._tilesX, 1 / this._tilesY );
		this._illumTexel.value.set( 1 / illumW, 1 / illumH );

		// Rebuild the materials: the gather shader bakes the direction count and both
		// materials capture the freshly-created uniform-array nodes.
		this._illuminationMaterial.dispose();
		this._gatherMaterial.dispose();
		this._illuminationMaterial = this._createIlluminationMaterial( this._debugMode );
		this._gatherMaterial = this._createGatherMaterial( this._debugMode );

		if ( this._environmentMaterial !== null ) {

			this._environmentMaterial.dispose();
			this._environmentMaterial = this._createEnvironmentMaterial( this._environmentRadiance );

		}

		// Deliberately DON'T reset(): the estimator is N-normalized (the gather's
		// `4/N`), so the converged brightness is the same for any direction count —
		// resetting would set frameCount to 0, and the next frame's `mix(prev, s, 1/1)`
		// would overwrite the converged lightmap with a single (very noisy at low N)
		// sample, which reads as a sudden darkening. Preserving the accumulation keeps
		// the brightness and just continues refining with the new tile layout. Use the
		// public `reset()` to restart accumulation from scratch on purpose.

	}

	/**
	 * Sets the self-intersection bias, in illumination-map-texel widths. Raise it
	 * if surfaces show self-illumination striations/acne; lower it if contact
	 * shadows leak. Default `1.5`.
	 *
	 * @param {number} texels - Bias in texel widths.
	 */
	setSelfBias( texels ) {

		if ( this.radiosity !== true ) return;

		this._selfBiasScale = texels;
		this._selfBias.value = ( 2 * this._sceneRadius.value / this._tileResolution ) * texels;
		this.reset();

	}

	/**
	 * Updates the emitted radiance (Le) of a lightmapped object at runtime by
	 * rewriting its per-vertex emissive attribute, then restarts accumulation.
	 *
	 * @param {Mesh} mesh - A mesh previously passed to {@link ProgressiveLightMap#addObjectsToLightMap}.
	 * @param {Color} emissive - The new emitted radiance.
	 */
	setEmissive( mesh, emissive ) {

		if ( this.radiosity !== true ) return;

		const attribute = mesh.geometry.getAttribute( 'radiosityEmissive' );
		if ( attribute === undefined ) return;

		for ( let i = 0; i < attribute.count; i ++ ) {

			attribute.setXYZ( i, emissive.r, emissive.g, emissive.b );

		}

		attribute.needsUpdate = true;
		this.reset();

	}

	/**
	 * Registers a surrounding environment mesh (e.g. a sky sphere) as a far,
	 * all-directions light source. It is rasterized into the illumination map each
	 * frame (so it illuminates the scene from every unoccluded direction) but does
	 * NOT expand the orthographic frusta: only the cameras' far plane is extended
	 * to reach it, while the lateral extent stays scene-sized. The mesh keeps its
	 * own material for display in the main scene.
	 *
	 * @param {Mesh} mesh - The environment mesh (rendered from the inside; BackSide).
	 * @param {Node<vec3>} radianceNode - The emitted radiance (e.g. a sky gradient).
	 */
	addEnvironment( mesh, radianceNode ) {

		if ( this.radiosity !== true ) return;

		this._environment = mesh;
		this._environmentEnabled = true;

		mesh.geometry.computeBoundingSphere();
		const scale = Math.max( mesh.scale.x, mesh.scale.y, mesh.scale.z );
		this._environmentRadius = mesh.geometry.boundingSphere.radius * scale;

		this._environmentRadiance = radianceNode;
		this._environmentMaterial = this._createEnvironmentMaterial( radianceNode );

		// Importance-sampling table: the sky mesh is unwrapped into an equirect
		// render target with the SAME radiance node, so any node (texture,
		// gradient, procedural) works and no separate texture is needed.
		if ( this._environmentTableMaterial !== null ) this._environmentTableMaterial.dispose();
		this._environmentTableMaterial = new NodeMaterial();
		this._environmentTableMaterial.side = DoubleSide;
		this._environmentTableMaterial.depthTest = false;
		this._environmentTableMaterial.depthWrite = false;
		this._environmentTableMaterial.vertexNode = vec4( equirectUV( positionLocal.normalize() ).mul( 2 ).sub( 1 ), 0, 1 );
		this._environmentTableMaterial.outputNode = vec4( radianceNode, 1 );

		this.updateEnvironmentImportance();

		this.reset();

	}

	/**
	 * (Re)builds the importance-sampling table from the environment's radiance
	 * node by rendering the unwrapped sky mesh into a small equirect target and
	 * reading it back (asynchronously). Call it again when the radiance changes,
	 * e.g. once a sky texture has finished loading.
	 *
	 * @return {Promise<void>} Resolves when the table is in use.
	 */
	async updateEnvironmentImportance() {

		if ( this.radiosity !== true || this._environment === null || this._environmentTableMaterial === null ) return;

		const renderer = this.renderer;
		const w = 128, h = 64;

		if ( this._environmentTableRT === null ) {

			this._environmentTableRT = new RenderTarget( w, h, { type: FloatType, minFilter: NearestFilter, magFilter: NearestFilter, generateMipmaps: false, depthBuffer: false } );

		}

		const env = this._environment;
		const oldParent = env.parent;
		const oldMaterial = env.material;
		const oldFrustumCulled = env.frustumCulled;
		const oldVisible = env.visible;
		const currentRenderTarget = renderer.getRenderTarget();
		const currentClearAlpha = renderer.getClearAlpha();
		renderer.getClearColor( this._clearColor );

		let buffer = null;

		try {

			env.material = this._environmentTableMaterial;
			env.frustumCulled = false;
			env.visible = true;
			this._environmentTableScene.add( env );

			renderer.setClearColor( 0x000000, 0 );
			renderer.setRenderTarget( this._environmentTableRT );
			renderer.clear();
			renderer.render( this._environmentTableScene, this._environmentTableCamera );
			renderer.setRenderTarget( currentRenderTarget );
			renderer.setClearColor( this._clearColor, currentClearAlpha );

			buffer = await renderer.readRenderTargetPixelsAsync( this._environmentTableRT, 0, 0, w, h );

		} catch ( e ) {

			console.warn( 'THREE.ProgressiveLightMap: environment importance table failed, using uniform sampling.', e );
			buffer = null;

		} finally {

			env.material = oldMaterial;
			env.frustumCulled = oldFrustumCulled;
			env.visible = oldVisible;
			if ( oldParent !== null ) oldParent.add( env ); else this._environmentTableScene.remove( env );

		}

		if ( buffer === null ) return;

		const lum = new Float32Array( w * h );
		// WebGPU render targets read back top row first (v = 1); WebGL bottom row first.
		const topFirst = renderer.backend !== undefined && renderer.backend.isWebGPUBackend === true;

		for ( let i = 0; i < h; i ++ ) {

			const src = topFirst ? h - 1 - i : i;

			for ( let j = 0; j < w; j ++ ) {

				const k = ( src * w + j ) * 4;
				lum[ i * w + j ] = 0.2126 * buffer[ k ] + 0.7152 * buffer[ k + 1 ] + 0.0722 * buffer[ k + 2 ];

			}

		}

		this._importanceTable = new EnvImportanceTable( lum, w, h );
		this._rebuildBaseDirections();
		this.reset();

	}

	/**
	 * Same stochastic-depth illumination shader as scene surfaces, but its
	 * radiance is the supplied environment colour and it renders BackSide.
	 *
	 * @private
	 * @param {Node<vec3>} radianceNode - The emitted radiance.
	 * @return {NodeMaterial} The environment illumination material.
	 */
	_createEnvironmentMaterial( radianceNode ) {

		const material = new NodeMaterial();
		material.side = BackSide;

		const d = this._directionArray.element( cameraIndex );
		const distance = dot( positionWorld.sub( this._originArray.element( cameraIndex ) ), d ).add( 1 );
		material.outputNode = vec4( radianceNode, distance );

		const seed = screenCoordinate.x
			.add( screenCoordinate.y.mul( 1931 ) )
			.add( this._frameSeed.mul( 17 ) )
			.add( dot( positionWorld, d ).mul( 7 ) );
		material.depthNode = hash( seed );

		return material;

	}

	/**
	 * Enables/disables the registered environment source.
	 *
	 * @param {boolean} enabled - Whether the environment illuminates the scene.
	 */
	setEnvironmentEnabled( enabled ) {

		if ( this.radiosity !== true || this._environment === null ) return;

		this._environmentEnabled = enabled;
		this.reset();

	}

	/**
	 * Flips the axes used when projecting a lightmap texel into the illumination
	 * map to sample it. The correct convention can differ between WebGPU backends
	 * (Dawn vs the WebGL/SwiftShader fallback), so this is exposed for tuning.
	 *
	 * @param {boolean} flipX - Flip the horizontal (U) axis.
	 * @param {boolean} flipY - Flip the vertical (V) axis.
	 */
	setProjectionFlip( flipX, flipY ) {

		if ( this.radiosity !== true ) return;

		this._projectionFlip.value.set( flipX ? - 1 : 1, flipY ? - 1 : 1 );
		this.reset();

	}

	/**
	 * Flips the axes used when reconstructing a sampled surface's world position
	 * from its illumination-map texel (the inverse of the projection mapping).
	 *
	 * @param {boolean} flipX - Flip the horizontal axis.
	 * @param {boolean} flipY - Flip the vertical axis.
	 */
	setReconstructionFlip( flipX, flipY ) {

		if ( this.radiosity !== true ) return;

		this._reconstructionFlip.value.set( flipX ? - 1 : 1, flipY ? - 1 : 1 );
		this.reset();

	}

	/**
	 * The texture holding the most recent radiosity solution (radiosity mode only).
	 *
	 * @type {?Texture}
	 */
	get lightMapTexture() {

		if ( this.radiosity !== true ) return null;

		return ( this._buffer1Active ? this._radiosityMap1 : this._radiosityMap2 ).texture;

	}

	/**
	 * Draws the lightmap in the main scene. Call this after adding the objects to it.
	 *
	 * @param {boolean} visible - Whether the debug plane should be visible
	 * @param {Vector3} [position] - Where the debug plane should be drawn
	 * @param {number} [size=100] - The side length of the debug plane.
	*/
	showDebugLightmap( visible, position = null, size = 100 ) {

		if ( this._lightMapContainers.length === 0 ) {

			console.warn( 'THREE.ProgressiveLightMap: Call .showDebugLightmap() after adding the objects.' );

			return;

		}

		if ( this._labelMesh === null ) {

			const source = this.radiosity === true ? this._lightMapSource : texture( this._progressiveLightMap1.texture );
			this._labelMesh = this._createDebugQuad( source.sample( uv().flipY() ), size );
			this._labelMesh.position.y = 250;

			this._lightMapContainers[ 0 ].object.parent.add( this._labelMesh );

		}

		if ( position !== null ) {

			this._labelMesh.position.copy( position );

		}

		this._labelMesh.visible = visible;

	}

	/**
	 * Draws the internal "illumination map" (the {@link ArrayCamera}'s tiled
	 * stochastic-depth render — one tile per global direction, RGB = radiance)
	 * in the main scene, for debugging the radiosity bounce.
	 *
	 * @param {boolean} visible - Whether the debug plane should be visible.
	 * @param {Vector3} [position] - Where the debug plane should be drawn.
	 * @param {number} [size=100] - The side length of the debug plane.
	 */
	showDebugIlluminationMap( visible, position = null, size = 100 ) {

		if ( this.radiosity !== true ) return;

		if ( this._lightMapContainers.length === 0 ) {

			console.warn( 'THREE.ProgressiveLightMap: Call .showDebugIlluminationMap() after adding the objects.' );

			return;

		}

		if ( this._illumDebugMesh === null ) {

			this._illumDebugMesh = this._createDebugQuad( texture( this._illuminationMap.texture ).sample( uv() ).rgb, size );
			this._illumDebugMesh.position.set( 0, 250, 0 );

			this._lightMapContainers[ 0 ].object.parent.add( this._illumDebugMesh );

		}

		if ( position !== null ) {

			this._illumDebugMesh.position.copy( position );

		}

		this._illumDebugMesh.visible = visible;

	}

	/**
	 * Creates an unlit, double-sided debug quad displaying the given color node.
	 *
	 * @private
	 * @param {Node} colorNode - The node to display.
	 * @param {number} size - The side length of the quad.
	 * @return {Mesh} The debug quad.
	 */
	_createDebugQuad( colorNode, size ) {

		const material = new MeshBasicNodeMaterial();
		material.colorNode = colorNode;
		material.side = DoubleSide;
		material.toneMapped = false;

		const mesh = new Mesh( new PlaneGeometry( size, size ), material );
		mesh.frustumCulled = false;

		return mesh;

	}

	/**
	 * Creates the Blurring Plane.
	 *
	 * @private
	 */
	_initializeBlurPlane() {

		const blurMaterial = new NodeMaterial();
		blurMaterial.polygonOffset = true;
		blurMaterial.polygonOffsetFactor = - 1;
		blurMaterial.polygonOffsetUnits = 3;

		blurMaterial.vertexNode = vec4( sub( uv(), vec2( 0.5 ) ).mul( 2 ), 1, 1 );

		const uvNode = uv().flipY().toVar();
		const pixelOffset = float( 0.5 ).div( float( this.resolution ) ).toVar();

		// Average only the valid (written) neighbours: including cleared texels
		// would pull the seams toward black, and NaN texels are dropped.
		const color = Fn( () => {

			const sum = vec4( 0 ).toVar();
			const count = float( 0 ).toVar();
			const offsets = [[ 1, 0 ], [ 0, 1 ], [ 0, - 1 ], [ - 1, 0 ], [ 1, 1 ], [ - 1, 1 ], [ 1, - 1 ], [ - 1, - 1 ]];

			for ( const [ ox, oy ] of offsets ) {

				const s = this._saneNode( this._previousShadowMap.sample( uvNode.add( vec2( pixelOffset.mul( ox ), pixelOffset.mul( oy ) ) ) ) ).toVar();
				If( s.a.greaterThan( 0 ), () => {

					sum.addAssign( s );
					count.addAssign( 1 );

				} );

			}

			return select( count.greaterThan( 0 ), sum.div( count ), vec4( 0 ) );

		} )();

		blurMaterial.fragmentNode = color;

		this._blurringPlane = new Mesh( new PlaneGeometry( 1, 1 ), blurMaterial );
		this._blurringPlane.name = 'Blurring Plane';
		this._blurringPlane.frustumCulled = false;
		this._blurringPlane.renderOrder = 0;
		this._blurringPlane.material.depthWrite = false;
		this._scene.add( this._blurringPlane );

	}

	// ---------------------------------------------------------------------------
	// Indirect radiosity (Stochastic Depth Buffering)
	// ---------------------------------------------------------------------------

	/**
	 * Allocates render targets, uniforms, the ArrayCamera and the shared
	 * illumination material for the radiosity solver.
	 *
	 * @private
	 * @param {Object} options - The radiosity options (see constructor).
	 */
	_initializeRadiosity( options ) {

		this._directions = options.directions !== undefined ? options.directions : 16;
		this._tileResolution = options.tileResolution !== undefined ? options.tileResolution : 256;
		this._gatherRadius = options.gatherRadius !== undefined ? Math.max( 1, Math.floor( options.gatherRadius ) ) : 1;
		this._maxHistory = options.maxHistory !== undefined ? options.maxHistory : 0;
		this._maxSamples = options.maxSamples !== undefined ? Math.max( 0, options.maxSamples ) : 0;
		this._fadeInSamples = options.fadeInSamples !== undefined ? Math.max( 0, options.fadeInSamples ) : 0;
		this._importance = options.envSampling !== 'uniform';
		this._importanceTable = null;
		this._needsClear = true;
		this._sceneBox = new Box3();

		this._frameCount = 0;
		this._clearColor = new Color();

		// Tile layout for the illumination map
		this._tilesX = Math.ceil( Math.sqrt( this._directions ) );
		this._tilesY = Math.ceil( this._directions / this._tilesX );
		const illumW = this._tilesX * this._tileResolution;
		const illumH = this._tilesY * this._tileResolution;

		// FloatType is required: the illumination map's alpha channel stores a
		// world-scale distance whose precision drives the "nearest surface along d"
		// comparison. The illumination map MUST use NearestFilter (the gather
		// reconstructs world positions from exact texels); the radiosity maps use
		// LinearFilter so the displayed lightmap is smoothly interpolated.
		this._illuminationMap = new RenderTarget( illumW, illumH, { type: FloatType, minFilter: NearestFilter, magFilter: NearestFilter, generateMipmaps: false, depthBuffer: true } );
		this._radiosityMap1 = new RenderTarget( this.resolution, this.resolution, { type: FloatType, minFilter: LinearFilter, magFilter: LinearFilter, generateMipmaps: false, depthBuffer: false } );
		this._radiosityMap2 = new RenderTarget( this.resolution, this.resolution, { type: FloatType, minFilter: LinearFilter, magFilter: LinearFilter, generateMipmaps: false, depthBuffer: false } );


		// CPU-side per-direction state (kept in sync with the uniform arrays)
		this._directionData = [];
		this._basisUData = [];
		this._basisVData = [];
		this._viewProjData = [];
		this._tileOffsetData = [];
		this._dirWeightData = [];
		this._originData = [];
		this._halfData = [];
		this._baseDirections = [];
		this._subCameras = [];

		const half = this._tileResolution * 0.5;

		for ( let k = 0; k < this._directions; k ++ ) {

			this._directionData.push( new Vector3( 0, 1, 0 ) );
			this._basisUData.push( new Vector3( 1, 0, 0 ) );
			this._basisVData.push( new Vector3( 0, 0, 1 ) );
			this._viewProjData.push( new Matrix4() );

			const tx = k % this._tilesX;
			const ty = Math.floor( k / this._tilesX );
			this._tileOffsetData.push( new Vector2( tx / this._tilesX, ty / this._tilesY ) );

			this._dirWeightData.push( 4 / this._directions );
			this._originData.push( new Vector3() );
			this._halfData.push( new Vector2( 1, 1 ) );

			const subCamera = new OrthographicCamera( - half, half, half, - half, 0, 1 );
			subCamera.viewport = new Vector4( tx * this._tileResolution, ty * this._tileResolution, this._tileResolution, this._tileResolution );
			this._subCameras.push( subCamera );

		}

		this._rebuildBaseDirections();
		this._arrayCamera = new ArrayCamera( this._subCameras );
		this._scene.add( this._arrayCamera );

		// Uniforms shared by the illumination and gather materials
		this._frameSeed = uniform( 0 );
		this._invSampleCount = uniform( 1 );
		this._fadeWeight = uniform( this._fadeInSamples > 0 ? 0 : 1 );
		this._debugDirection = uniform( 0, 'int' );
		this._debugMode = null; // null | 'direction' | 'direct'

		// Runtime-toggleable axis flips for the illumination-map projection (sampling)
		// and the world-position reconstruction. Components are +1 (no flip) or -1
		// (flip). The V (Y) axis is flipped by default to match the WebGPU
		// render-target sampling convention (same as shadow maps' `y.oneMinus()`).
		// Exposed for tuning because the correct values can differ between backends
		// (e.g. the WebGL/SwiftShader fallback).
		this._projectionFlip = uniform( new Vector2( 1, - 1 ) );
		this._reconstructionFlip = uniform( new Vector2( 1, - 1 ) );

		// Self-intersection bias. `_selfBiasScale` is in illumination-map-texel
		// widths; `_selfBias` is its value in world units (set in _computeBounds).
		this._selfBiasScale = 1.5;
		this._selfBias = uniform( 0.1 );

		// Optional environment (sky sphere): a far, all-directions source captured by
		// the illumination pass without expanding the ortho frusta.
		this._environment = null;
		this._environmentMaterial = null;
		this._environmentRadiance = null;
		this._environmentTableMaterial = null;
		this._environmentTableRT = null;
		this._environmentTableScene = new Scene();
		this._environmentTableCamera = new OrthographicCamera( - 1, 1, 1, - 1, 0, 1 );
		this._environmentRadius = 0;
		this._environmentEnabled = false;
		this._sceneCenter = uniform( new Vector3() );
		this._sceneRadius = uniform( 1 );
		this._tileScale = uniform( new Vector2( 1 / this._tilesX, 1 / this._tilesY ) );
		this._illumTexel = uniform( new Vector2( 1 / illumW, 1 / illumH ) );

		this._directionArray = uniformArray( this._directionData, 'vec3' );
		this._basisUArray = uniformArray( this._basisUData, 'vec3' );
		this._basisVArray = uniformArray( this._basisVData, 'vec3' );
		this._viewProjArray = uniformArray( this._viewProjData );
		this._tileOffsetArray = uniformArray( this._tileOffsetData, 'vec2' );
		this._dirWeightArray = uniformArray( this._dirWeightData, 'float' );
		this._originArray = uniformArray( this._originData, 'vec3' );
		this._halfArray = uniformArray( this._halfData, 'vec2' );

		// Texture nodes whose .value we swap each frame (ping-pong)
		this._lightMapSource = texture( this._radiosityMap1.texture ); // current radiosity (display + illumination source)
		this._prevRadiosity = texture( this._radiosityMap1.texture ); // previous radiosity (progressive mean)
		this._illuminationTex = texture( this._illuminationMap.texture );

		// All three runtime materials are shared across objects. Per-object surface
		// data (albedo / emission) travels through constant vertex attributes, which
		// bind per-geometry through a shared material — unlike per-object material
		// uniforms, whose buffers the backend may merge when materials are identical.
		this._illuminationMaterial = this._createIlluminationMaterial();
		this._gatherMaterial = this._createGatherMaterial();

		this._displayMaterial = new MeshBasicNodeMaterial();
		this._displayMaterial.colorNode = this._lightMapSource.sample( uv( 1 ) ).mul( this._fadeWeight );

	}

	/**
	 * Per-object radiosity setup: bakes the surface's albedo (ρ) and emitted
	 * radiance (Le) into constant vertex attributes (read by the shared gather
	 * material) and switches the object to the shared display material.
	 *
	 * @private
	 * @param {Object} container - The lightmap container `{ object, basicMat }`.
	 */
	_initializeRadiosityObject( container ) {

		const geometry = container.object.geometry;
		const sourceMat = container.object.material;

		const albedo = sourceMat.color || new Color( 0xffffff );
		const emissive = new Color().copy( sourceMat.emissive || new Color( 0x000000 ) )
			.multiplyScalar( sourceMat.emissiveIntensity !== undefined ? sourceMat.emissiveIntensity : 1 );

		// Per-vertex constant attributes carry ρ and Le through the shared material.
		const count = geometry.attributes.position.count;
		const albedoArray = new Float32Array( count * 3 );
		const emissiveArray = new Float32Array( count * 3 );

		for ( let i = 0; i < count; i ++ ) {

			albedoArray[ i * 3 + 0 ] = albedo.r;
			albedoArray[ i * 3 + 1 ] = albedo.g;
			albedoArray[ i * 3 + 2 ] = albedo.b;

			emissiveArray[ i * 3 + 0 ] = emissive.r;
			emissiveArray[ i * 3 + 1 ] = emissive.g;
			emissiveArray[ i * 3 + 2 ] = emissive.b;

		}

		geometry.setAttribute( 'radiosityAlbedo', new BufferAttribute( albedoArray, 3 ) );
		geometry.setAttribute( 'radiosityEmissive', new BufferAttribute( emissiveArray, 3 ) );

		container.object.material = this._displayMaterial;

	}

	/**
	 * The stochastic-depth illumination material (shared by all objects). Renders
	 * geometry normally with the ArrayCamera; each fragment writes its current
	 * outgoing radiance and signed distance along its tile's direction, with a
	 * random fragment depth so overlapping surfaces shuffle per frame.
	 *
	 * @private
	 * @param {?string} [mode=null] - Debug source override: `'direct'` stores
	 * emission only (so the gather yields shadowed direct lighting, the initial
	 * source of the solve); `'direction'` stores a static albedo+emission
	 * appearance (so the single-direction reprojection is stable and non-recursive);
	 * `null` stores the fed-back radiosity (the full bounce).
	 * @return {NodeMaterial} The illumination material.
	 */
	_createIlluminationMaterial( mode = null ) {

		const material = new NodeMaterial();
		material.side = DoubleSide;

		// The current direction for the sub-camera being rasterized
		const d = this._directionArray.element( cameraIndex );

		// Outgoing radiance stored per surface, depending on debug mode.
		const emissive = attribute( 'radiosityEmissive', 'vec3' );
		const outgoing = mode === 'direct'
			? emissive
			: mode === 'direction'
				? attribute( 'radiosityAlbedo', 'vec3' ).add( emissive )
				: this._saneNode( this._lightMapSource.sample( uv( 1 ) ) ).rgb;

		// Store the distance along d from the tile's near plane, biased by +1 so
		// that the clear value (0) can be rejected as "no surface" during the gather.
		const distance = dot( positionWorld.sub( this._originArray.element( cameraIndex ) ), d ).add( 1 );

		material.outputNode = vec4( outgoing, distance );

		// Stochastic depth in [0,1): each fragment along a pixel's ray gets a random
		// depth so overlapping surfaces shuffle which one "wins" per frame; over
		// frames the 3×3/5×5 gather neighbourhood then statistically represents all
		// surfaces along the ray. We BLUE-NOISE this rather than white-noise it: a
		// per-surface rank (stable across the frame, so each surface keeps its slot)
		// is offset by a spatially blue-noise interleaved-gradient dither that is
		// animated per frame by the golden ratio. Because the dither is shared by all
		// fragments at a pixel, surfaces keep their relative ordering while the winner
		// is decorrelated across neighbouring texels and over frames — markedly
		// cleaner convergence than an i.i.d. hash.
		const surfaceRank = hash( dot( positionWorld, d ).mul( 7 ) );
		const ign = screenCoordinate.x.mul( 0.06711056 )
			.add( screenCoordinate.y.mul( 0.00583715 ) ).fract().mul( 52.9829189 ).fract();
		const dither = ign.add( this._frameSeed.mul( 0.61803399 ) ).fract();
		material.depthNode = surfaceRank.add( dither ).fract();

		return material;

	}

	/**
	 * Gathers the radiance of the nearest in-hemisphere surface seen from `p`
	 * (normal `n`) along global direction `i` — the paper's neighborhood search in
	 * the illumination map. Returns `vec3(0)` when nothing is visible. This is the
	 * shared core of both the production loop and the single-direction debug view.
	 *
	 * @private
	 */
	_gatherDirection( p, n, i ) {

		const radius = this._gatherRadius;
		const out = vec3( 0 ).toVar();
		const d = this._directionArray.element( i ).toVar();

		If( dot( n, d ).greaterThan( 0 ), () => {

			// Project the texel into this direction's tile (with tunable axis flips
			// to match the render-target sampling convention of the active backend).
			const clip = this._viewProjArray.element( i ).mul( vec4( p, 1 ) ).toVar();
			const ndc = clip.xy.div( clip.w );
			const baseUv = ndc.mul( this._projectionFlip ).mul( 0.5 ).add( 0.5 ).toVar();
			const tileOffset = this._tileOffsetArray.element( i ).toVar();

			// Snap the projected coordinate to the illumination-map texel grid so the
			// texel that NearestFilter returns and the world position we reconstruct
			// from it refer to the SAME surface (otherwise they differ by up to half a
			// texel, which makes the nearest-surface test flip → regular striations).
			const tileUv = baseUv.mul( this._tileScale ).add( tileOffset )
				.div( this._illumTexel ).floor().add( 0.5 ).mul( this._illumTexel ).toVar();

			const basisU = this._basisUArray.element( i ).toVar();
			const basisV = this._basisVArray.element( i ).toVar();
			const pAlongD = dot( p, d ).toVar();

			const best = float( 1e20 ).toVar();
			const bestRGB = vec3( 0 ).toVar();

			// Guard band so the neighborhood never leaks into adjacent tiles
			const lo = tileOffset.add( this._illumTexel ).toVar();
			const hi = tileOffset.add( this._tileScale ).sub( this._illumTexel ).toVar();

			for ( let oy = - radius; oy <= radius; oy ++ ) {

				for ( let ox = - radius; ox <= radius; ox ++ ) {

					const sampleUv = tileUv.add( this._illumTexel.mul( vec2( ox, oy ) ) ).clamp( lo, hi ).toVar();
					const sample = this._illuminationTex.sample( sampleUv ).toVar();
					const w = sample.w;

					If( w.greaterThan( 0.5 ), () => { // reject "no surface" texels

						// Reconstruct the sample's world position (orthographic ray),
						// applying the matching reconstruction flips.
						const localNdc = sampleUv.sub( tileOffset ).div( this._tileScale ).mul( 2 ).sub( 1 ).mul( this._reconstructionFlip ).toVar();
						const origin = this._originArray.element( i );
						const half = this._halfArray.element( i );
						const base = origin
							.add( basisU.mul( localNdc.x.mul( half.x ) ) )
							.add( basisV.mul( localNdc.y.mul( half.y ) ) );
						const samplePos = base.add( d.mul( w.sub( 1 ) ) ).toVar();

						// Reject the receiver's own surface and near-coplanar self hits
						// (a slope-style bias ~ one illumination-map texel in world units),
						// which otherwise alias into regular self-illumination stripes.
						If( dot( samplePos.sub( p ), n ).greaterThan( this._selfBias ), () => {

							const diff = dot( samplePos, d ).sub( pAlongD ).abs(); // closest surface to p along d
							If( diff.lessThan( best ), () => {

								best.assign( diff );
								bestRGB.assign( sample.rgb );

							} );

						} );

					} );

				}

			}

			If( best.lessThan( 1e19 ), () => out.assign( bestRGB ) );

		} );

		return out;

	}

	/**
	 * Builds the shared gather material implementing
	 * `Lo = Le + 4·ρ·(1/N)·Σ Lk·max(dot(n,dk),0)` with the paper's neighborhood
	 * sampling and a progressive cumulative-mean blend. Per-surface albedo (ρ) and
	 * emission (Le) come from constant vertex attributes, so a single material
	 * serves every object.
	 *
	 * @private
	 * @param {?string} [mode=null] - `'direction'` builds the single-direction debug
	 * view (reprojects only direction `debugDirection` and writes the raw gathered
	 * radiance — no cosine / albedo / accumulation — to verify the lightmap ↔
	 * array-camera projection and occlusion). `'direct'` and `null` use the full
	 * production gather (they differ only in the illumination source: emission-only
	 * vs fed-back radiosity).
	 * @return {NodeMaterial} The gather material.
	 */
	_createGatherMaterial( mode = null ) {

		const material = new NodeMaterial();
		material.side = DoubleSide;
		material.depthTest = false;
		material.depthWrite = false;

		// Per-surface diffuse reflectance (ρ) and emitted radiance (Le)
		const albedoNode = attribute( 'radiosityAlbedo', 'vec3' );
		const emissiveNode = attribute( 'radiosityEmissive', 'vec3' );

		// Unwrap geometry into atlas (uv1) space
		const uvNode = uv( 1 ).flipY();
		material.vertexNode = vec4( sub( uvNode, vec2( 0.5 ) ).mul( 2 ), 1, 1 );

		const N = this._directions;

		if ( mode === 'direction' ) {

			material.fragmentNode = Fn( () => {

				const p = positionWorld.toVar();
				const n = normalWorld.normalize().toVar();
				return vec4( this._gatherDirection( p, n, this._debugDirection ), 1 );

			} )();

			return material;

		}

		material.fragmentNode = Fn( () => {

			const p = positionWorld.toVar();
			// A zero-length normal would normalize to NaN and poison the map: treat it as up.
			const nLen = length( normalWorld ).toVar();
			const n = select( nLen.greaterThan( 1e-8 ), normalWorld.div( nLen ), vec3( 0, 1, 0 ) ).toVar();
			const accum = vec3( 0 ).toVar();

			Loop( { start: int( 0 ), end: int( N ), type: 'int', condition: '<' }, ( { i } ) => {

				const nDotD = dot( n, this._directionArray.element( i ) );
				// Monte-Carlo weight 1/(N·π·pdf_k): 4/N for uniform sphere sampling, the
				// balance-heuristic mixture pdf for environment importance sampling.
				accum.addAssign( this._gatherDirection( p, n, i ).mul( nDotD ).mul( this._dirWeightArray.element( i ) ) );

			} );

			// Le + ρ·Σ w_k·Lk·cos  (the weights carry the 4/N resp. 1/(N·π·pdf) of the estimator)
			const indirect = albedoNode.mul( accum );
			const sample = this._saneNode( vec4( emissiveNode.add( indirect ), 1 ) );

			// Progressive cumulative-mean integration; the first sample overwrites so
			// garbage in a fresh render target can never leak in through 0·previous.
			const previous = this._saneNode( this._prevRadiosity.sample( uv( 1 ) ) );
			return select( this._invSampleCount.greaterThanEqual( 1 ), sample, mix( previous, sample, this._invSampleCount ) );

		} )();

		return material;

	}

	/**
	 * Enables/disables the single-direction debug view. With a valid index the
	 * displayed lightmap shows — for that one (frozen) global direction — the
	 * nearest surface each texel sees along it (a static albedo+emission
	 * appearance), so you can confirm the lightmap ↔ array-camera projection and
	 * occlusion against {@link ProgressiveLightMap#showDebugIlluminationMap}.
	 *
	 * @param {?number} index - Direction index `[0, directions)`, or `null`/`-1` to disable.
	 */
	setDebugDirection( index ) {

		const enabled = index !== null && index >= 0;
		if ( enabled ) this._debugDirection.value = Math.min( index, this._directions - 1 );
		this._setDebugMode( enabled ? 'direction' : ( this._debugMode === 'direction' ? null : this._debugMode ) );

	}

	/**
	 * Enables/disables the direct-lighting debug view. When enabled, the displayed
	 * lightmap shows only the **direct** illumination — the first bounce gathered
	 * from the emissive (light) surfaces, with the technique's soft stochastic
	 * shadows — which is the initial source the full radiosity solve builds on.
	 *
	 * @param {boolean} enabled - Whether to show direct lighting only.
	 */
	setDebugDirectLighting( enabled ) {

		this._setDebugMode( enabled ? 'direct' : ( this._debugMode === 'direct' ? null : this._debugMode ) );

	}

	/**
	 * Rebuilds the illumination and gather materials for the given debug mode.
	 *
	 * @private
	 * @param {?string} mode - `null` | `'direction'` | `'direct'`.
	 */
	_setDebugMode( mode ) {

		if ( this.radiosity !== true || mode === this._debugMode ) return;

		this._debugMode = mode;

		this._illuminationMaterial.dispose();
		this._gatherMaterial.dispose();
		this._illuminationMaterial = this._createIlluminationMaterial( mode );
		this._gatherMaterial = this._createGatherMaterial( mode );

		this.reset();

	}

	/**
	 * Computes the bounding sphere of all lightmapped objects and configures the
	 * orthographic sub-cameras' depth range accordingly.
	 *
	 * @private
	 */
	_computeBounds() {

		_box.makeEmpty();

		for ( const container of this._lightMapContainers ) {

			_box.expandByObject( container.object );

		}

		_box.getBoundingSphere( _sphere );
		this._sceneBox.copy( _box );

		this._sceneCenter.value.copy( _sphere.center );
		this._sceneRadius.value = Math.max( _sphere.radius, 1e-3 );

		// `_selfBiasScale` illumination-map texels, in world units (the ortho views
		// span 2·radius across tileResolution texels).
		this._selfBias.value = ( 2 * this._sceneRadius.value / this._tileResolution ) * this._selfBiasScale;

	}

	/**
	 * Refreshes the global directions (a randomly-rotated Fibonacci sphere for
	 * low-discrepancy, progressively-converging coverage) and rebuilds the
	 * matching sub-camera frustums, viewports and gather uniform arrays.
	 *
	 * @private
	 */
	_updateDirections() {

		const center = this._sceneCenter.value;
		const radius = this._sceneRadius.value;
		const box = this._sceneBox;
		const envEnabled = this._environment !== null && this._environmentEnabled;
		const N = this._directions;
		const table = this._importance && envEnabled ? this._importanceTable : null;
		const nUniform = this._uniformCount();
		const nImportance = N - nUniform;
		const uniformPdf = 1 / ( 4 * Math.PI );

		// Rotate the whole direction set each frame so it explores all directions
		// over time. We use a LOW-DISCREPANCY rotation sequence (stratified over
		// frames, ~O(1/n) error decay) rather than i.i.d. random rotations
		// (~O(1/√n)) — same cost, strictly faster convergence. In single-direction
		// debug the set is frozen so the inspected direction stays fixed.
		if ( this._debugMode === 'direction' ) _quaternion.identity();
		else this._stratifiedRotation( _quaternion, this._frameCount );

		// Lateral/depth margin of two illumination-map texels around the bounds.
		const margin = ( 2 * radius / this._tileResolution ) * 2;

		for ( let k = 0; k < N; k ++ ) {

			const d = this._directionData[ k ];

			if ( k < nUniform || table === null ) {

				d.copy( this._baseDirections[ k % this._baseDirections.length ] ).applyQuaternion( _quaternion ).normalize();

			} else {

				// Low-discrepancy 2D point (R2 sequence) for this frame and slot.
				const n = this._frameCount * nImportance + ( k - nUniform );
				const u1 = ( 0.5 + 0.7548776662466927 * n ) % 1;
				const u2 = ( 0.5 + 0.5698402909980532 * n ) % 1;
				table.sample( u1, u2, d );

			}

			// Balance heuristic over the 50/50 mixture of uniform and environment pdfs.
			this._dirWeightData[ k ] = table !== null
				? 1 / ( N * Math.PI * ( 0.5 * uniformPdf + 0.5 * table.pdf( d ) ) )
				: 4 / N;

			const camera = this._subCameras[ k ];
			camera.up.set( 0, 1, 0 );
			if ( Math.abs( d.y ) > 0.99 ) camera.up.set( 0, 0, 1 );
			camera.position.copy( center ).addScaledVector( d, - 1 );
			camera.lookAt( center );
			camera.updateMatrixWorld();

			const bu = this._basisUData[ k ].setFromMatrixColumn( camera.matrixWorld, 0 ).normalize();
			const bv = this._basisVData[ k ].setFromMatrixColumn( camera.matrixWorld, 1 ).normalize();

			// Fit the orthographic frustum to the bounds as seen along d (a tight
			// box fit, not the bounding sphere): more illumination texels per surface.
			let minU = Infinity, maxU = - Infinity, minV = Infinity, maxV = - Infinity, minD = Infinity, maxD = - Infinity;

			for ( let c = 0; c < 8; c ++ ) {

				_corner.set( c & 1 ? box.max.x : box.min.x, c & 2 ? box.max.y : box.min.y, c & 4 ? box.max.z : box.min.z ).sub( center );
				const pu = _corner.dot( bu ), pv = _corner.dot( bv ), pd = _corner.dot( d );
				if ( pu < minU ) minU = pu;
				if ( pu > maxU ) maxU = pu;
				if ( pv < minV ) minV = pv;
				if ( pv > maxV ) maxV = pv;
				if ( pd < minD ) minD = pd;
				if ( pd > maxD ) maxD = pd;

			}

			const halfU = ( maxU - minU ) / 2 + margin;
			const halfV = ( maxV - minV ) / 2 + margin;
			const near = minD - margin;

			// Far plane: reach the environment (sky) when enabled, WITHOUT widening the
			// frustum (left/right/top/bottom stay scene-sized); otherwise just the scene.
			const far = envEnabled
				? ( radius + this._environmentRadius ) * 1.05 - near
				: maxD + margin - near;

			// Camera on the near plane, centred on the fitted extent.
			camera.position.copy( center )
				.addScaledVector( bu, ( minU + maxU ) / 2 )
				.addScaledVector( bv, ( minV + maxV ) / 2 )
				.addScaledVector( d, near );
			this._originData[ k ].copy( camera.position );
			this._halfData[ k ].set( halfU, halfV );
			camera.lookAt( _target.copy( camera.position ).add( d ) );

			camera.left = - halfU;
			camera.right = halfU;
			camera.top = halfV;
			camera.bottom = - halfV;
			camera.near = 0;
			camera.far = far;
			camera.updateMatrixWorld();
			camera.updateProjectionMatrix();

			// Camera basis: column 0 = right (U), column 1 = up (V)
			this._basisUData[ k ].setFromMatrixColumn( camera.matrixWorld, 0 ).normalize();
			this._basisVData[ k ].setFromMatrixColumn( camera.matrixWorld, 1 ).normalize();

			this._viewProjData[ k ].multiplyMatrices( camera.projectionMatrix, camera.matrixWorldInverse );

		}

		// The uniform arrays re-upload from these mutated objects automatically
		// (NodeUpdateType.RENDER), so no explicit invalidation is required.

	}

	/**
	 * Writes a low-discrepancy uniform rotation for sample index `n` into `q`.
	 * Uses the 3D R3 additive-recurrence sequence (generator = the "plastic
	 * number" root of x⁴ = x + 1) to produce a stratified point in [0,1)³, then
	 * Shoemake's map to a uniformly-distributed unit quaternion. Successive `n`
	 * cover SO(3) far more evenly than i.i.d. random rotations, so the radiosity
	 * estimate's directional variance decays faster across frames.
	 *
	 * @private
	 * @param {Quaternion} q - Target quaternion (written in place).
	 * @param {number} n - Sample index (frame counter).
	 */
	_stratifiedRotation( q, n ) {

		// α_i = (1/φ₃)^i, φ₃ ≈ 1.220744 (root of x⁴ = x + 1)
		const u1 = ( 0.5 + 0.8191725133961644 * n ) % 1;
		const u2 = ( 0.5 + 0.6710436067037893 * n ) % 1;
		const u3 = ( 0.5 + 0.5497004779019702 * n ) % 1;

		const s1 = Math.sqrt( 1 - u1 );
		const s2 = Math.sqrt( u1 );
		const t2 = 2 * Math.PI * u2;
		const t3 = 2 * Math.PI * u3;

		q.set( s1 * Math.sin( t2 ), s1 * Math.cos( t2 ), s2 * Math.sin( t3 ), s2 * Math.cos( t3 ) );

	}

	/**
	 * Returns the k-th point of an N-point Fibonacci sphere (unit vector).
	 *
	 * @private
	 * @param {number} k - The point index.
	 * @param {number} n - The total number of points.
	 * @return {Vector3} The direction.
	 */
	_fibonacciSphere( k, n ) {

		const golden = Math.PI * ( 3 - Math.sqrt( 5 ) );
		const y = 1 - ( k / ( n - 1 || 1 ) ) * 2;
		const r = Math.sqrt( Math.max( 0, 1 - y * y ) );
		const phi = k * golden;

		return new Vector3( Math.cos( phi ) * r, y, Math.sin( phi ) * r );

	}

}

/**
 * Discrete 2D distribution over an equirectangular luminance table, for drawing
 * world directions proportional to environment radiance. Uses the `equirectUV`
 * convention (u from atan2(z, x), v from asin(y)).
 *
 * @private
 */
class EnvImportanceTable {

	constructor( lum, w, h ) {

		this.w = w;
		this.h = h;
		this.dLat = Math.PI / h;
		this.dLon = ( 2 * Math.PI ) / w;

		const weights = new Float32Array( w * h );
		const rowCdf = new Float32Array( h );
		const condCdf = new Float32Array( w * h );
		let total = 0;

		for ( let i = 0; i < h; i ++ ) {

			const cosLat = Math.cos( ( ( i + 0.5 ) / h - 0.5 ) * Math.PI );
			let rowSum = 0;

			for ( let j = 0; j < w; j ++ ) {

				const wt = ( Math.max( lum[ i * w + j ], 0 ) + 1e-4 ) * cosLat;
				weights[ i * w + j ] = wt;
				rowSum += wt;
				condCdf[ i * w + j ] = rowSum;

			}

			for ( let j = 0; j < w; j ++ ) condCdf[ i * w + j ] /= rowSum;
			total += rowSum;
			rowCdf[ i ] = total;

		}

		for ( let i = 0; i < h; i ++ ) rowCdf[ i ] /= total;

		this.weights = weights;
		this.total = total;
		this.rowCdf = rowCdf;
		this.condCdf = condCdf;

	}

	/**
	 * Builds a table from an equirectangular texture (image element, bitmap,
	 * canvas or data texture), downsampled to at most 256×128.
	 */
	static fromTexture( texture ) {

		const image = texture.image;
		if ( ! image ) return null;

		let w, h, lum;
		const srgbToLinear = ( c ) => ( c <= 0.04045 ? c / 12.92 : Math.pow( ( c + 0.055 ) / 1.055, 2.4 ) );

		if ( image.data !== undefined && image.width !== undefined ) {

			// DataTexture: float (linear) or 8-bit (sRGB) data
			w = image.width;
			h = image.height;
			const data = image.data;
			const channels = data.length / ( w * h );
			const is8bit = data instanceof Uint8Array || data instanceof Uint8ClampedArray;
			lum = new Float32Array( w * h );

			for ( let i = 0; i < w * h; i ++ ) {

				let r = data[ i * channels ], g = data[ i * channels + 1 ], b = data[ i * channels + 2 ];
				if ( is8bit ) {

					r = srgbToLinear( r / 255 ); g = srgbToLinear( g / 255 ); b = srgbToLinear( b / 255 );

				}

				lum[ i ] = 0.2126 * r + 0.7152 * g + 0.0722 * b;

			}

		} else {

			const srcW = image.naturalWidth || image.videoWidth || image.width;
			const srcH = image.naturalHeight || image.videoHeight || image.height;
			if ( ! srcW || ! srcH ) return null;
			w = Math.min( 256, srcW );
			h = Math.min( 128, srcH );
			const canvas = document.createElement( 'canvas' );
			canvas.width = w;
			canvas.height = h;
			const ctx = canvas.getContext( '2d' );
			ctx.drawImage( image, 0, 0, w, h );
			const data = ctx.getImageData( 0, 0, w, h ).data;
			lum = new Float32Array( w * h );

			for ( let i = 0; i < w * h; i ++ ) {

				lum[ i ] = 0.2126 * srgbToLinear( data[ i * 4 ] / 255 ) + 0.7152 * srgbToLinear( data[ i * 4 + 1 ] / 255 ) + 0.0722 * srgbToLinear( data[ i * 4 + 2 ] / 255 );

			}

		}

		// Row 0 of the table is v = 0 (y = -1). With flipY the image's first row is v = 1.
		if ( texture.flipY ) {

			const flipped = new Float32Array( w * h );
			for ( let i = 0; i < h; i ++ ) flipped.set( lum.subarray( ( h - 1 - i ) * w, ( h - i ) * w ), i * w );
			lum = flipped;

		}

		return new EnvImportanceTable( lum, w, h );

	}

	/** Draws a direction for the 2D sample (u1, u2) into `out`; returns its pdf per solid angle. */
	sample( u1, u2, out ) {

		const w = this.w, h = this.h;
		let i = lowerBound( this.rowCdf, 0, h, u1 );
		if ( i >= h ) i = h - 1;
		const r0 = i > 0 ? this.rowCdf[ i - 1 ] : 0;
		const r1 = this.rowCdf[ i ];
		const xi = r1 > r0 ? Math.min( 0.999999, ( u1 - r0 ) / ( r1 - r0 ) ) : 0.5;
		const base = i * w;
		let j = lowerBound( this.condCdf, base, base + w, u2 ) - base;
		if ( j >= w ) j = w - 1;
		const c0 = j > 0 ? this.condCdf[ base + j - 1 ] : 0;
		const c1 = this.condCdf[ base + j ];
		const zeta = c1 > c0 ? Math.min( 0.999999, ( u2 - c0 ) / ( c1 - c0 ) ) : 0.5;
		const v = ( i + xi ) / h;
		const u = ( j + zeta ) / w;
		const lat = ( v - 0.5 ) * Math.PI;
		const lon = ( u - 0.5 ) * 2 * Math.PI;
		const cosLat = Math.cos( lat );
		out.set( cosLat * Math.cos( lon ), Math.sin( lat ), cosLat * Math.sin( lon ) );
		return this._pdfCell( i, j );

	}

	/** Pdf per solid angle of an arbitrary direction. */
	pdf( dir ) {

		const v = Math.asin( Math.max( - 1, Math.min( 1, dir.y ) ) ) / Math.PI + 0.5;
		let u = Math.atan2( dir.z, dir.x ) / ( 2 * Math.PI ) + 0.5;
		if ( u < 0 ) u += 1;
		if ( u >= 1 ) u -= 1;
		const i = Math.min( this.h - 1, Math.floor( v * this.h ) );
		const j = Math.min( this.w - 1, Math.floor( u * this.w ) );
		return this._pdfCell( i, j );

	}

	_pdfCell( i, j ) {

		const cosLat = Math.max( Math.cos( ( ( i + 0.5 ) / this.h - 0.5 ) * Math.PI ), 1e-4 );
		return this.weights[ i * this.w + j ] / this.total / ( cosLat * this.dLat * this.dLon );

	}

}

/** First index in [lo, hi) whose cdf value is >= u. */
function lowerBound( cdf, lo, hi, u ) {

	while ( lo < hi ) {

		const mid = ( lo + hi ) >> 1;
		if ( cdf[ mid ] < u ) lo = mid + 1;
		else hi = mid;

	}

	return lo;

}

export { ProgressiveLightMap };
