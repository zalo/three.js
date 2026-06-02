import { BackSide, Box3, BufferAttribute, Color, DoubleSide, FloatType, HalfFloatType, LinearFilter, NearestFilter, ArrayCamera, OrthographicCamera, Matrix4, Quaternion, Sphere, Vector2, Vector3, Vector4, PlaneGeometry, Mesh, RenderTarget, Scene, MeshBasicNodeMaterial, MeshPhongNodeMaterial, NodeMaterial } from 'three/webgpu';
import { add, attribute, cameraIndex, dot, float, Fn, hash, If, int, Loop, mix, normalWorld, output, positionWorld, screenCoordinate, sub, texture, uniform, uniformArray, uv, vec2, vec3, vec4 } from 'three/tsl';

import { potpack } from '../libs/potpack.module.js';

const _sphere = /*@__PURE__*/ new Sphere();
const _box = /*@__PURE__*/ new Box3();
const _quaternion = /*@__PURE__*/ new Quaternion();

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

		if ( this.radiosity !== true || this._blurringPlane === null ) {

			return;

		}

		const renderer = this.renderer;

		// Store renderer state we are about to override
		const currentRenderTarget = renderer.getRenderTarget();
		const currentPixelRatio = renderer.getPixelRatio();
		const currentClearAlpha = renderer.getClearAlpha();
		renderer.getClearColor( this._clearColor );

		// Advance the progressive estimator
		this._frameCount ++;
		this._frameSeed.value = this._frameCount;
		const n = this._maxHistory > 0 ? Math.min( this._frameCount, this._maxHistory ) : this._frameCount;
		this._invSampleCount.value = 1 / n;

		// Refresh the global directions and their ArrayCamera tiles
		this._updateDirections();

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

		const readRT = this._buffer1Active ? this._radiosityMap1 : this._radiosityMap2;
		const writeRT = this._buffer1Active ? this._radiosityMap2 : this._radiosityMap1;

		// 1) ILLUMINATION PASS — rasterize N global directions into tiles of one map.
		//    Each surface stores its current outgoing radiance (rgb) and its signed
		//    distance along the tile's direction (a), with a random fragment depth.
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

		// 2) GATHER PASS — for every lightmap texel, gather the nearest in-hemisphere
		//    surface along each direction, weight by cos and albedo, add emission,
		//    and fold into the radiosity map with the progressive mean.
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

		// Restore the objects' display material and original scene
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

		renderer.setRenderTarget( currentRenderTarget );

	}

	/**
	 * Resets the progressive radiosity accumulation. Call this whenever the
	 * scene, materials or lighting change so the estimator restarts cleanly.
	 */
	reset() {

		this._frameCount = 0;

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

		// Same stochastic-depth illumination shader as scene surfaces, but its
		// radiance is the supplied environment colour and it renders BackSide.
		const material = new NodeMaterial();
		material.side = BackSide;

		const d = this._directionArray.element( cameraIndex );
		const distance = dot( positionWorld.sub( this._sceneCenter ), d ).add( this._sceneRadius ).add( 1 );
		material.outputNode = vec4( radianceNode, distance );

		const seed = screenCoordinate.x
			.add( screenCoordinate.y.mul( 1931 ) )
			.add( this._frameSeed.mul( 17 ) )
			.add( dot( positionWorld, d ).mul( 7 ) );
		material.depthNode = hash( seed );

		this._environmentMaterial = material;

		this.reset();

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

		const color = add(
			this._previousShadowMap.sample( uvNode.add( vec2( pixelOffset, 0 ) ) ),
			this._previousShadowMap.sample( uvNode.add( vec2( 0, pixelOffset ) ) ),
			this._previousShadowMap.sample( uvNode.add( vec2( 0, pixelOffset.negate() ) ) ),
			this._previousShadowMap.sample( uvNode.add( vec2( pixelOffset.negate(), 0 ) ) ),
			this._previousShadowMap.sample( uvNode.add( vec2( pixelOffset, pixelOffset ) ) ),
			this._previousShadowMap.sample( uvNode.add( vec2( pixelOffset.negate(), pixelOffset ) ) ),
			this._previousShadowMap.sample( uvNode.add( vec2( pixelOffset, pixelOffset.negate() ) ) ),
			this._previousShadowMap.sample( uvNode.add( vec2( pixelOffset.negate(), pixelOffset.negate() ) ) ),
		).div( 8 );

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

			this._baseDirections.push( this._fibonacciSphere( k, this._directions ) );

			const subCamera = new OrthographicCamera( - half, half, half, - half, 0, 1 );
			subCamera.viewport = new Vector4( tx * this._tileResolution, ty * this._tileResolution, this._tileResolution, this._tileResolution );
			this._subCameras.push( subCamera );

		}

		this._arrayCamera = new ArrayCamera( this._subCameras );
		this._scene.add( this._arrayCamera );

		// Uniforms shared by the illumination and gather materials
		this._frameSeed = uniform( 0 );
		this._invSampleCount = uniform( 1 );
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
		this._displayMaterial.colorNode = this._lightMapSource.sample( uv( 1 ) );

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
				: this._lightMapSource.sample( uv( 1 ) ).rgb;

		// Store dot(p - center, d) biased to (0, 2R] so that the clear value (0)
		// can be rejected as "no surface" during the gather.
		const distance = dot( positionWorld.sub( this._sceneCenter ), d ).add( this._sceneRadius ).add( 1 );

		material.outputNode = vec4( outgoing, distance );

		// Stochastic depth: a per-pixel, per-frame random value in [0,1).
		// Decorrelated by frame and depth-along-d so winners shuffle and converge.
		const seed = screenCoordinate.x
			.add( screenCoordinate.y.mul( 1931 ) )
			.add( this._frameSeed.mul( 17 ) )
			.add( dot( positionWorld, d ).mul( 7 ) );
		material.depthNode = hash( seed );

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
						const base = this._sceneCenter
							.add( basisU.mul( localNdc.x.mul( this._sceneRadius ) ) )
							.add( basisV.mul( localNdc.y.mul( this._sceneRadius ) ) );
						const samplePos = base.add( d.mul( w.sub( this._sceneRadius ).sub( 1 ) ) ).toVar();

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
			const n = normalWorld.normalize().toVar();
			const accum = vec3( 0 ).toVar();

			Loop( { start: int( 0 ), end: int( N ), type: 'int', condition: '<' }, ( { i } ) => {

				const nDotD = dot( n, this._directionArray.element( i ) );
				accum.addAssign( this._gatherDirection( p, n, i ).mul( nDotD ) );

			} );

			// Le + 4·ρ·(1/N)·Σ Lk·cos  (the 4 and the 1/π of the diffuse BRDF combine)
			const indirect = albedoNode.mul( accum ).mul( 4 / N );
			const sample = emissiveNode.add( indirect );

			// Progressive cumulative-mean integration
			const previous = this._prevRadiosity.sample( uv( 1 ) ).rgb;
			return vec4( mix( previous, sample, this._invSampleCount ), 1 );

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

		// Far plane: reach the environment (sky) when enabled, WITHOUT widening the
		// frustum (left/right/top/bottom stay scene-sized); otherwise just the scene.
		const far = ( this._environment !== null && this._environmentEnabled )
			? ( radius + this._environmentRadius ) * 1.05
			: 2 * radius;

		// A fresh random rotation each frame keeps the set stratified while
		// exploring all directions over time. In single-direction debug the set is
		// frozen so the inspected direction stays fixed.
		if ( this._debugMode === 'direction' ) _quaternion.identity();
		else _quaternion.random();

		for ( let k = 0; k < this._directions; k ++ ) {

			const d = this._directionData[ k ].copy( this._baseDirections[ k ] ).applyQuaternion( _quaternion ).normalize();

			const camera = this._subCameras[ k ];
			camera.position.copy( center ).addScaledVector( d, - radius );
			camera.up.set( 0, 1, 0 );
			if ( Math.abs( d.y ) > 0.99 ) camera.up.set( 0, 0, 1 );
			camera.lookAt( center );

			camera.left = - radius;
			camera.right = radius;
			camera.top = radius;
			camera.bottom = - radius;
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

export { ProgressiveLightMap };
