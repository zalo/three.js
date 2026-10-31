import { DepthTexture, FloatType, HalfFloatType, Matrix4, NodeMaterial, QuadMesh, RenderTarget, StorageBufferAttribute, Vector2, Vector3 } from 'three/webgpu';
import { Break, Fn, If, Loop, abs, atan, atomicAdd, atomicLoad, atomicMax, atomicMin, atomicStore, cameraPosition, ceil, cos, cross, diffuseColor, dot, equirectUV, exp2, float, floor, fract, getViewPosition, hash, instanceIndex, int, ivec2, ivec3, ivec4, log2, max, min, mrt, normalWorldGeometry, normalize, output, positionWorld, screenUV, select, sin, sqrt, storage, texture, uint, uniform, uv, uvec3, vec2, vec3, vec4, velocity } from 'three/tsl';

import { RayTracedRadiosity } from './RayTracedRadiosity.js';

// empty slots of the hash tables, and missing probes

const EMPTY = 0xffffffff;
const NONE = 0xffffffff;

// the most slots a hash table lookup or insertion probes, the tables are at most a quarter full

const MAX_PROBES = 64;

// the deposits of a frame are summed as fixed point integers, since WGSL has no float atomics: the
// radiance per channel, the rays that pass the interval and the rays of a bin

const SCALE = 2 ** 14;
const WORDS = 5;

// a bin takes at most this many rays per frame, so that the sums of the clamped radiance of its rays fit in
// 32 bits: close to the camera, a probe covers thousands of pixels

const MAX_RAYS = 512;

// surfaces select their cells from a point this share of the spacing in front of them: the positions of the
// G-buffer come from a depth buffer, so a surface on a cell boundary, e.g. a floor at y = 0, would select the
// cells on both sides of it from pixel to pixel

const KEY_OFFSET = 0.25;

// fixed point increments of the R2 low-discrepancy sequence, as in chandra

const R2 = [ 3242174889, 2447445413 ];

// the texels of the octahedral irradiance maps of the probes along each axis

const IRRADIANCE_SIZE = 6;
const IRRADIANCE_TEXELS = IRRADIANCE_SIZE * IRRADIANCE_SIZE;

// the counters of a frame, per cascade: the slots allocated, the slots freed for the next frame and the probes;
// then the probes allocated in the finest cascade and the rays of the coarsest probes

const ALLOCATED = 0, FREED = 1, PROBES = 2;

const _position = /*@__PURE__*/ new Vector3();
const _size = /*@__PURE__*/ new Vector2();

/**
 * Real-time diffuse global illumination with "Split Radiance Cascades" (Freeman, Sannikov and Pitt-Francis,
 * 2026), following its reference implementation [chandra]{@link https://github.com/entropylost/chandra} as
 * closely as WebGPU allows.
 *
 * Every frame:
 *
 * - The scene is rendered into a G-buffer of the lit color, the diffuse color, the geometry normal, the
 * velocity and depth.
 * - Every visible surface allocates the probe of its cell in sparse hash tables, one per cascade. The cells
 * grow with the Chebyshev distance to the camera in levels of detail, and near the boundary of a level the
 * surfaces use both levels ("blending radius"). Secondary probes, two levels coarser, sit at the hits of the
 * rays of the last frame. The coarser cascades halve the cells of the finer ones.
 * - The probes keep their slot, and so their history, while they live: primary probes as long as a pixel
 * lands in them, secondary ones for 10 frames after a hit landed in them, coarser ones as long as a finer
 * probe in them does. Freed slots go to a free list.
 * - Every pixel traces a ray through a BVH of the scene, see {@link RayTracedRadiosity}, in a direction of an
 * R2 sequence over the rays of its probes, and every hit of the last frame traces a secondary ray. A hit adds
 * its emission, the light of the suns with a shadow ray and the irradiance of its secondary probe ("infinite
 * bounces"). A ray adds its radiance to the cascade whose interval holds its hit distance, and its
 * transmittance to the cascades below ("ray splitting").
 * - The bins sum their rays, and decay their sums by `qualityDecay` every frame. The probes count their rays
 * likewise ("quality"), which weighs them in the merge and the final gather.
 * - The cascades are merged from the coarsest one down, and the probes of the finest one are projected into
 * octahedral maps of their irradiance, which the pixels interpolate.
 *
 * Differences from chandra, due to WebGPU: the hash tables only hold 32-bit keys, with the cells modulo 512,
 * and are rebuilt every frame with lock-free insertions by `atomicMin()`, since TSL has no compare-exchange;
 * the probes find their slot of the last frame in the last frame's table. The deposits are summed in fixed
 * point, since WGSL has no float atomics, and the radiance of a ray is clamped to `maxRadiance`. Rays start
 * a share of the probe spacing off the surface, since the G-buffer positions come from a depth buffer.
 *
 * The options are properties that can change at any time, they apply in the next `update()`: the ones of the
 * probe layout and of the levels of detail (`spacing`, `levelDistance`, `levels`, `cascades`, `directions`,
 * `maxProbes`, `orientations`) clear the probes and rebuild the passes, `rayStride`,
 * `secondary` and `useQualityWeight` rebuild the passes, the others are uniforms. Fetch `getCompositeNode()`
 * again after the passes were rebuilt.
 *
 * The geometry of the scene is static, see `setScene()`. The suns, directional lights, and the camera can
 * change every frame. The direct light of the analytic lights is left to the materials of the G-buffer;
 * emissive surfaces and the sky light the scene only through the cascades.
 *
 * This class can only be used with {@link WebGPURenderer} and a WebGPU backend.
 *
 * @three_import import { RealtimeRadianceCascades } from 'three/addons/lighting/RealtimeRadianceCascades.js';
 */
class RealtimeRadianceCascades {

	/**
	 * Constructs new real-time radiance cascades.
	 *
	 * @param {Object} [options] - The options.
	 * @param {number} [options.spacing=0.0625] - The probe spacing of the finest cascade in the first level of detail, in world units.
	 * @param {number} [options.levelDistance=4] - The Chebyshev distance from the camera that the first level of detail covers, every
	 * further level doubles it and the spacing. `2 * levelDistance / spacing` must stay below 510.
	 * @param {number} [options.levels=16] - The levels of detail, at most 16, surfaces beyond the last one get no light.
	 * @param {number} [options.blendingRadius=1.1] - Surfaces within this factor of the distance of the next level of detail use both
	 * levels, `1` uses one level, `2` blends across the whole level.
	 * @param {number} [options.cascades=4] - The cascades.
	 * @param {number} [options.directions=4] - The directions of the finest cascade along the polar axis, there are twice as many
	 * around it, so 4 gives 32 directions. Every cascade doubles them along both axes.
	 * @param {number} [options.maxProbes=32768] - The most probes of the finest cascade, every coarser one has 1.25 / 3^n as many.
	 * @param {number} [options.rayStride=1] - The pixels of a block of this size take turns at tracing a ray, `1` traces a ray from
	 * every pixel, as in chandra.
	 * @param {boolean} [options.secondary=true] - Whether the hits of the rays get the light of secondary probes.
	 * @param {number} [options.qualityDecay=0.97] - The factor of the sums of the bins and of the ray counts of the probes per frame.
	 * @param {number} [options.minDirectionWeight=2] - The sums of a bin with at most this many rays do not decay.
	 * @param {boolean} [options.useQualityWeight=true] - Whether the probes are weighted by their ray counts in the merge and the
	 * final gather.
	 * @param {number} [options.maxLifetime=1] - The frames a primary probe lives after a ray of a pixel was deposited into it.
	 * @param {boolean} [options.orientations=false] - Whether the probes of a cell are split by the dominant axis of the normals, so
	 * that e.g. a floor and a wall meeting in a cell do not share probes. chandra does not split them.
	 * @param {number} [options.secondaryLifetime=10] - The frames a secondary probe lives after a hit landed in it.
	 * @param {number} [options.maxRadiance=500] - The radiance of a ray is clamped to this, for the fixed point sums.
	 * @param {boolean} [options.adaptiveLOD=true] - Whether the levels of detail coarsen while the cascades are nearly full, so that
	 * views that need more probes than `maxProbes` get coarser probes instead of none, see `lodBias`.
	 */
	constructor( { spacing = 0.0625, levelDistance = 4, levels = 16, blendingRadius = 1.1, cascades = 4, directions = 4, maxProbes = 32768, rayStride = 1, secondary = true, qualityDecay = 0.97, minDirectionWeight = 2, useQualityWeight = true, maxLifetime = 1, secondaryLifetime = 10, orientations = false, maxRadiance = 500, adaptiveLOD = true } = {} ) {

		this._blendingOctaves = uniform( 0 );
		this._maxLifetime = uniform( 1, 'uint' );
		this._secondaryLifetime = uniform( 10, 'uint' );

		this.spacing = spacing;
		this.levelDistance = levelDistance;
		this.levels = levels;
		this.blendingRadius = blendingRadius;
		this.cascades = cascades;
		this.directions = directions;
		this.maxProbes = maxProbes;
		this.rayStride = rayStride;
		this.secondary = secondary;
		this.useQualityWeight = useQualityWeight;
		this.maxLifetime = maxLifetime;
		this.secondaryLifetime = secondaryLifetime;
		this.orientations = orientations;
		this.adaptiveLOD = adaptiveLOD;

		/**
		 * The octaves added to the distances of the levels of detail. With `adaptiveLOD`, it rises while a cascade
		 * wants more than 85% of its probes, by half of the octaves that would bring it to 75%, and falls by 0.01 while
		 * all have less than 60%, after the probe counts of a frame are read back. A level of detail has about a
		 * quarter of the probes of the one before, and the blending of the levels hides the fractional steps.
		 *
		 * @type {number}
		 * @default 0
		 */
		this.lodBias = 0;

		/**
		 * The factor of the interval lengths between cascades.
		 *
		 * @type {number}
		 * @default 4
		 */
		this.lengthScale = 4;

		/**
		 * The length of the interval of the finest cascade over its probe spacing: the spacing of the ends of
		 * its rays then equals the probe spacing.
		 *
		 * @type {number}
		 */
		this.intervalScale = 0;

		/**
		 * The bytes of the storage buffers.
		 *
		 * @type {number}
		 */
		this.memory = 0;

		/**
		 * The probes of each cascade in the last frame, see `readStats()`.
		 *
		 * @type {Array<number>}
		 */
		this.probes = [];

		/**
		 * The most probes of each cascade.
		 *
		 * @type {Array<number>}
		 */
		this.capacities = [];

		/**
		 * The probes of the finest cascade allocated in the last frame, see `readStats()`.
		 *
		 * @type {number}
		 */
		this.newProbes = 0;

		/**
		 * What the composite shows: `0` the lit scene, `1` the indirect light only, `2` the irradiance,
		 * `3` the probes of the finest cascade, `4` the levels of detail and their cells.
		 *
		 * @type {UniformNode<int>}
		 */
		this.mode = uniform( 0, 'int' );

		/**
		 * The radiance of the sky without an environment map.
		 *
		 * @type {UniformNode<color>}
		 */
		this.skyColor = uniform( new Vector3( 0, 0, 0 ) );

		/**
		 * The scale of the indirect light in the composite.
		 *
		 * @type {UniformNode<float>}
		 */
		this.intensity = uniform( 1 );

		this._radiosity = null;
		this._skyTarget = null;
		this._gbuffer = null;
		this._hits = null;
		this._kernels = null;
		this._frameKernels = null;
		this._compositeNode = null;
		this._width = 0;
		this._height = 0;
		this._frame = 0;
		this._layout = null;
		this._buffers = [];
		this._bufferKey = null;
		this._kernelKey = null;
		this._hitCapacity = 0;

		this._parity = uniform( 0, 'uint' );
		this._frameNode = uniform( 0, 'uint' );
		this._jitter = uniform( new Vector2() );
		this._cameraPosition = uniform( new Vector3() );
		this._projectionInverse = uniform( new Matrix4() );
		this._cameraWorld = uniform( new Matrix4() );
		this._resolution = uniform( new Vector2() );
		this._qualityDecay = uniform( qualityDecay );
		this._minDirectionWeight = uniform( minDirectionWeight );
		this._maxRadiance = uniform( maxRadiance );
		this._sunDirection = uniform( new Vector3( 0, 1, 0 ) );
		this._sunColor = uniform( new Vector3() );
		this._lodBias = uniform( 0 );
		this._readingCounts = false;
		this._screenWidth = uniform( 1, 'uint' );
		this._screenHeight = uniform( 1, 'uint' );
		this._blocksX = uniform( 1, 'uint' );
		this._blocks = uniform( 1, 'uint' );

		this._applyChanges();

	}

	/**
	 * Surfaces within this factor of the distance of the next level of detail use both levels, `1` uses one level,
	 * `2` blends across the whole level.
	 *
	 * @type {number}
	 */
	get blendingRadius() {

		return this._blendingRadius;

	}

	set blendingRadius( value ) {

		this._blendingRadius = value;
		this._blendingOctaves.value = Math.log2( Math.max( value, 1 ) );

	}

	/**
	 * The factor of the sums of the bins and of the ray counts of the probes per frame: the light of the probes is
	 * an average over about `1 / ( 1 - qualityDecay )` frames, 33 for chandra's 0.97. Lower values follow changes of
	 * the light faster and are noisier. Every bounce of the secondary probes adds such a delay.
	 *
	 * @type {number}
	 */
	get qualityDecay() {

		return this._qualityDecay.value;

	}

	set qualityDecay( value ) {

		this._qualityDecay.value = value;

	}

	/**
	 * The sums of a bin with at most this many rays do not decay, so that bins with few rays keep their light.
	 *
	 * @type {number}
	 */
	get minDirectionWeight() {

		return this._minDirectionWeight.value;

	}

	set minDirectionWeight( value ) {

		this._minDirectionWeight.value = value;

	}

	/**
	 * The frames a primary probe lives after a ray of a pixel was deposited into it.
	 *
	 * @type {number}
	 */
	get maxLifetime() {

		return this._maxLifetime.value;

	}

	set maxLifetime( value ) {

		this._maxLifetime.value = Math.max( Math.round( value ), 1 );

	}

	/**
	 * The frames a secondary probe lives after a hit landed in it.
	 *
	 * @type {number}
	 */
	get secondaryLifetime() {

		return this._secondaryLifetime.value;

	}

	set secondaryLifetime( value ) {

		this._secondaryLifetime.value = Math.max( Math.round( value ), 1 );

	}

	/**
	 * Builds the BVH of the scene that the rays trace. Call it again after changing the geometry or the
	 * materials.
	 *
	 * @param {WebGPURenderer} renderer - The renderer.
	 * @param {Scene} scene - The scene.
	 * @param {?ArrayBuffer} [bvhData=null] - The BVH of the scene from `serializeBVH()`, which is used instead of
	 * building it if it was made for the same geometry.
	 */
	setScene( renderer, scene, bvhData = null ) {

		this._radiosity?.dispose();

		this._radiosity = new RayTracedRadiosity( { bounces: 0 } );
		this._radiosity.shadeHits = true;
		this._radiosity.bvhData = bvhData;
		this._radiosity.update( renderer, scene );
		this._radiosity.bvhData = null;

		this._createSky( renderer );

		// the probes of the last scene are cleared, and the passes trace the new BVH

		this._bufferKey = this._kernelKey = null;
		this._applyChanges();

	}

	/**
	 * Returns the BVH of the scene of the last `setScene()` in a binary form, to pass to `setScene()` instead of
	 * building it again, e.g. stored next to the model of the scene.
	 *
	 * @return {ArrayBuffer} The BVH.
	 */
	serializeBVH() {

		return this._radiosity.serializeBVH();

	}

	/**
	 * Returns a texture of the G-buffer: `'output'`, `'albedo'`, `'normal'`, `'velocity'` or `'depth'`, e.g. for
	 * {@link TRAANode}.
	 *
	 * @param {string} name - The name of the texture.
	 * @return {TextureNode} The texture.
	 */
	getTextureNode( name ) {

		const gbuffer = this._getGBuffer( 1, 1 );

		return texture( name === 'depth' ? gbuffer.depthTexture : gbuffer.textures.find( ( map ) => map.name === name ) );

	}

	/**
	 * Returns the node that composites the lit scene and the indirect light, e.g. for the `outputNode` of a
	 * {@link RenderPipeline}.
	 *
	 * @return {Node<vec4>} The composite.
	 */
	getCompositeNode() {

		this._applyChanges();

		if ( this._compositeNode ) return this._compositeNode;

		const gbuffer = this._getGBuffer( 1, 1 );
		const color = texture( gbuffer.textures[ 0 ] );
		const albedo = texture( gbuffer.textures[ 1 ] );
		const normalMap = texture( gbuffer.textures[ 2 ] );
		const depthMap = texture( gbuffer.depthTexture );

		this._compositeNode = Fn( () => {

			const lit = color.sample( screenUV ).rgb.toVar();
			const result = lit.toVar();
			const depth = depthMap.sample( screenUV ).x.toVar();

			If( depth.lessThan( 1 ), () => {

				const position = this._worldPosition( screenUV, depth ).toVar();
				const normal = this._facingNormal( normalMap.sample( screenUV ).xyz ).toVar();
				const diffuse = albedo.sample( screenUV ).rgb.toVar();
				const radiance = this._gather( position, normal ).mul( this.intensity ).toVar();

				If( this.mode.equal( 0 ), () => {

					result.assign( lit.add( diffuse.mul( radiance ) ) );

				} ).ElseIf( this.mode.equal( 1 ), () => {

					result.assign( diffuse.mul( radiance ) );

				} ).ElseIf( this.mode.equal( 2 ), () => {

					result.assign( radiance );

				} ).ElseIf( this.mode.equal( 3 ), () => {

					// the probe of the cell

					const level = this._primaryLevel( position ).toVar();
					const slot = uint( NONE ).toVar();

					If( level.greaterThanEqual( 0 ), () => {

						slot.assign( this._lookup( 0, this._keyOf( this._surfaceCell( position, normal, level, 0 ), level ), this._groupOf( normal ) ) );

					} );

					result.assign( select( slot.equal( NONE ), vec3( 1, 0, 1 ), vec3( hash( slot ), hash( slot.add( 7 ) ), hash( slot.add( 13 ) ) ) ) );

				} ).ElseIf( this.mode.equal( 4 ), () => {

					// a color per level of detail, with the edges of the cells

					const level = this._primaryLevel( position ).toVar();
					const tint = cos( vec3( 0, 0.33, 0.67 ).add( float( level ).mul( 0.17 ) ).mul( 2 * Math.PI ) ).mul( 0.5 ).add( 0.5 );
					const spacing = this._spacingOf( level.max( 0 ), 0 ).toVar();
					const f = fract( position.add( normal.mul( spacing.mul( KEY_OFFSET ) ) ).div( spacing ) ).toVar();
					const edge = min( f.oneMinus(), f ).toVar();
					const line = select( min( edge.x, min( edge.y, edge.z ) ).lessThan( 0.03 ), float( 0.4 ), float( 1 ) );

					result.assign( select( level.greaterThanEqual( 0 ), tint.mul( line ).mul( lit.dot( vec3( 0.3, 0.6, 0.1 ) ).min( 1 ).mul( 0.6 ).add( 0.4 ) ), vec3( 0 ) ) );

				} );

			} );

			return vec4( result, 1 );

		} )();

		return this._compositeNode;

	}

	/**
	 * Renders the G-buffer, then allocates the probes, traces the rays, and accumulates and merges the cascades
	 * for a frame.
	 *
	 * @param {WebGPURenderer} renderer - The renderer.
	 * @param {Scene} scene - The scene.
	 * @param {Camera} camera - The camera.
	 */
	update( renderer, scene, camera ) {

		this._applyChanges();

		const size = renderer.getDrawingBufferSize( _size );
		const width = Math.max( Math.floor( size.x ), 1 ), height = Math.max( Math.floor( size.y ), 1 );
		const gbuffer = this._getGBuffer( width, height );
		const pixels = width * height;

		// the passes read the size of the screen from uniforms, only the buffer of the hits of the pixels grows

		if ( width !== this._width || height !== this._height ) {

			gbuffer.setSize( width, height );
			this._width = width;
			this._height = height;

		}

		if ( pixels > this._hitCapacity ) {

			this._hitCapacity = Math.ceil( pixels * 1.25 );
			this._disposeKernels( false );

		}

		const stride = this.rayStride;
		const blocks = Math.ceil( width / stride ) * Math.ceil( height / stride );

		this._screenWidth.value = width;
		this._screenHeight.value = height;
		this._blocksX.value = Math.ceil( width / stride );
		this._blocks.value = blocks;

		if ( this._kernels === null ) this._kernels = this._createKernels();
		if ( this._frameKernels === null ) this._frameKernels = this._createFrameKernels();

		// the G-buffer

		const previousTarget = renderer.getRenderTarget();
		const previousMRT = renderer.getMRT();

		renderer.setRenderTarget( gbuffer );
		renderer.setMRT( this._mrt );
		renderer.render( scene, camera );
		renderer.setMRT( previousMRT );
		renderer.setRenderTarget( previousTarget );

		// the uniforms of the frame

		camera.updateMatrixWorld();

		this._frame ++;
		this._frameNode.value = this._frame;
		this._parity.value = this._frame & 1;

		this._cameraPosition.value.setFromMatrixPosition( camera.matrixWorld );
		this._projectionInverse.value.copy( camera.projectionMatrixInverse );
		this._cameraWorld.value.copy( camera.matrixWorld );
		this._resolution.value.set( width, height );

		// the random offset of the directions of the frame, pcg3df( tick, 29, 968 ) in chandra

		const random = pcg3d( this._frame, 29, 968 );
		this._jitter.value.set( random[ 0 ] / 0xffffffff, random[ 1 ] / 0xffffffff );

		this._updateSun( scene );

		this._lodBias.value = this.lodBias;

		// the passes

		const kernels = this._kernels;
		const frameKernels = this._frameKernels;
		const uids = this._uids = {};

		// the passes are grouped by stage for timestamp queries, see readTimings()

		const run = ( stage, kernel, count = null ) => {

			if ( kernel === null ) return;

			renderer.compute( kernel, count );

			if ( renderer.backend.trackTimestamp === true ) ( uids[ stage ] ??= [] ).push( renderer.backend.getTimestampUID( kernel ) );

		};

		run( 'allocate', kernels.clear );
		run( 'allocate', frameKernels.gather, pixels );
		run( 'allocate', frameKernels.gatherSecondary, pixels );
		run( 'allocate', kernels.keep );
		run( 'allocate', kernels.assign[ 0 ] );

		for ( let n = 1; n < this.cascades; n ++ ) {

			run( 'allocate', kernels.fill[ n ] );
			run( 'allocate', kernels.assign[ n ] );

		}

		for ( let n = 0; n < this.cascades; n ++ ) run( 'allocate', kernels.free[ n ] );

		// the offsets of the rays of the probes in the R2 sequence

		run( 'allocate', frameKernels.count, blocks );

		for ( let n = 1; n < this.cascades; n ++ ) run( 'allocate', kernels.countUp[ n ] );
		for ( let n = this.cascades - 1; n >= 0; n -- ) run( 'allocate', kernels.offsets[ n ] );

		run( 'trace', frameKernels.traceSecondary, pixels );
		run( 'trace', frameKernels.trace, blocks );

		for ( let n = 0; n < this.cascades; n ++ ) run( 'accumulate', kernels.accumulate[ n ] );
		for ( let n = this.cascades - 1; n >= 0; n -- ) run( 'merge', kernels.merge[ n ] );

		run( 'resolve', kernels.irradiance );

		if ( this.adaptiveLOD ) this._adaptLOD( renderer );

	}

	/**
	 * Reads back the probe counts of this frame, one read at a time, and adapts `lodBias` to how full the cascades
	 * are (after zalo's adaptive LOD bias for chandra).
	 *
	 * @private
	 * @param {WebGPURenderer} renderer - The renderer.
	 */
	_adaptLOD( renderer ) {

		if ( this._readingCounts ) return;

		this._readingCounts = true;

		const base = ( this._frame & 1 ) * this._layout.counters;
		const previous = ( ( this._frame + 1 ) & 1 ) * this._layout.counters;
		const counters = this._counters;
		const capacities = this.capacities;
		const cascades = this.cascades;

		renderer.getArrayBufferAsync( counters.value ).then( ( buffer ) => {

			this._readingCounts = false;

			if ( this._counters !== counters ) return;

			// the probes a cascade wanted: the ones it has, and the new ones beyond the free slots of the last frame

			const counts = new Uint32Array( buffer );
			let fullness = 0;

			for ( let n = 0; n < cascades; n ++ ) {

				const failed = Math.max( counts[ base + ALLOCATED * cascades + n ] - counts[ previous + FREED * cascades + n ], 0 );

				fullness = Math.max( fullness, ( counts[ base + PROBES * cascades + n ] + failed ) / capacities[ n ] );

			}

			// a level of detail has about a quarter of the probes of the one before: half of the octaves that would
			// bring the cascades to 75%, at least 0.05

			if ( fullness > 0.85 ) this.lodBias += Math.max( Math.log2( fullness / 0.75 ) / 4, 0.05 );
			else if ( fullness < 0.6 ) this.lodBias = Math.max( this.lodBias - 0.01, 0 );

		} ).catch( () => {

			// disposed

		} );

	}

	/**
	 * Reads back the probe counts of the cascades into `probes`, and the probes allocated in the finest one into
	 * `newProbes`.
	 *
	 * @param {WebGPURenderer} renderer - The renderer.
	 * @return {Promise<Array<number>>} The counts.
	 */
	async readStats( renderer ) {

		const counters = new Uint32Array( await renderer.getArrayBufferAsync( this._counters.value ) );
		const base = ( this._frame & 1 ) * this._layout.counters;

		this.probes = Array.from( { length: this.cascades }, ( _, n ) => counters[ base + PROBES * this.cascades + n ] );
		this.newProbes = counters[ base + 3 * this.cascades ];

		return this.probes;

	}

	/**
	 * Returns the GPU time of the stages of the last frame in milliseconds: the allocation of the probes,
	 * the rays, the accumulation, the merge and the irradiance maps. The renderer must track timestamps.
	 *
	 * @param {WebGPURenderer} renderer - The renderer.
	 * @return {Promise<?Object<string, number>>} The times, or null if some are missing.
	 */
	async readTimings( renderer ) {

		const uids = this._uids ?? {};

		await renderer.resolveTimestampsAsync( 'compute' );

		// other readers of the timestamps, e.g. the inspector, may have taken some of them

		const pool = renderer.backend.timestampQueryPool.compute;
		const timings = {};

		for ( const stage in uids ) {

			if ( uids[ stage ].some( ( uid ) => pool?.hasTimestampQuery( uid ) !== true ) ) return null;

			timings[ stage ] = uids[ stage ].reduce( ( total, uid ) => total + pool.getTimestamp( uid ), 0 );

		}

		return timings;

	}

	/**
	 * Frees all internal resources.
	 */
	dispose() {

		this._radiosity?.dispose();
		this._skyTarget?.dispose();
		this._gbuffer?.dispose();

		this._disposeKernels();
		this._disposeBuffers();

	}

	/**
	 * Applies changed options: rebuilds the buffers if the layout of the probes or the levels of detail changed,
	 * which clears the probes, and the passes if they depend on a changed option.
	 *
	 * @private
	 */
	_applyChanges() {

		const bufferKey = [ this.spacing, this.levelDistance, this.levels, this.cascades, this.directions, this.maxProbes, this.orientations ].join();
		const kernelKey = [ bufferKey, this.rayStride, this.secondary, this.useQualityWeight ].join();

		if ( kernelKey === this._kernelKey ) return;

		if ( bufferKey !== this._bufferKey ) {

			// the cells of a level of detail must span less than 512 cells along each axis, see _keyOf()

			const span = 2 * this.levelDistance / this.spacing + 4;

			if ( span >= 510 ) throw new Error( 'THREE.RealtimeRadianceCascades: The levels of detail span too many cells, increase spacing or decrease levelDistance.' );
			if ( this.levels > 16 ) throw new Error( 'THREE.RealtimeRadianceCascades: At most 16 levels of detail.' );

			this._disposeBuffers();

			this.intervalScale = Math.sqrt( 2 * this.directions * this.directions / ( 4 * Math.PI ) );
			this._layout = this._createLayout();
			this.capacities = this._layout.cascades.map( ( cascade ) => cascade.capacity );
			this._createBuffers();
			this._bufferKey = bufferKey;

		}

		this._disposeKernels();
		this._compositeNode = null;
		this._kernelKey = kernelKey;

	}

	/**
	 * Frees the storage buffers of the probes.
	 *
	 * @private
	 */
	_disposeBuffers() {

		for ( const buffer of this._buffers ) buffer.value.dispose();

		this._buffers = [];
		this.memory = 0;

	}

	/**
	 * Frees the pipelines of the passes, and the per pixel buffer of the passes that depend on the size of the
	 * screen. They are created again in the next update.
	 *
	 * @private
	 * @param {boolean} [all=true] - Whether to free the passes that do not depend on the size of the screen too.
	 */
	_disposeKernels( all = true ) {

		for ( const kernels of all ? [ this._kernels, this._frameKernels ] : [ this._frameKernels ] ) {

			for ( const kernel of Object.values( kernels ?? {} ).flat() ) kernel?.dispose();

		}

		if ( all ) this._kernels = null;

		this._frameKernels = null;
		this._hits?.value.dispose();
		this._hits = null;

	}

	/**
	 * Returns the sizes and offsets of the cascades in the buffers.
	 *
	 * @private
	 * @return {Object} The layout.
	 */
	_createLayout() {

		const cascades = [];
		const groups = this.orientations ? 6 : 1;
		let tables = 0, slots = 0, bins = 0, merged = 0;

		for ( let n = 0; n < this.cascades; n ++ ) {

			const theta = this.directions * 2 ** n;
			// a coarser cell holds about four finer probes on a surface, but fewer at edges, on thin geometry and with
			// the orientations split: about three in the courtyard

			const capacity = n === 0 ? this.maxProbes : Math.max( Math.ceil( this.maxProbes * 1.25 / 3 ** n ), 64 );
			// a table per orientation, which holds a share of the probes

			const tableSize = 2 ** Math.ceil( Math.log2( capacity * ( groups > 1 ? 2 : 4 ) ) );
			const binCount = 2 * theta * theta;

			// the merged radiance of the finest cascade at full resolution, of the coarser ones averaged over the
			// four bins within each bin of the next finer one

			const outputs = n === 0 ? binCount : binCount / 4;

			cascades.push( { theta, capacity, tableSize, bins: binCount, outputs, tableOffset: tables, slotOffset: slots, binOffset: bins, mergedOffset: merged } );

			tables += tableSize * groups;
			slots += capacity;
			bins += capacity * binCount;
			merged += capacity * outputs;

		}

		return { cascades, groups, tables, slots, bins, merged, counters: 3 * this.cascades + 2 };

	}

	/**
	 * Creates the storage buffers. The probes keep their slots while they live, the hash tables alternate
	 * between the current frame's and the last frame's.
	 *
	 * @private
	 */
	_createBuffers() {

		const { cascades, tables, slots, bins, merged, counters } = this._layout;

		const make = ( array, type, itemSize, atomic = false ) => {

			const node = storage( new StorageBufferAttribute( array, itemSize ), type, array.length / itemSize );

			return atomic ? node.toAtomic() : node;

		};

		// the free lists start with all slots, and the counts of free slots of both frames with the capacities

		const freeList = new Uint32Array( slots );
		const counterArray = new Uint32Array( counters * 2 );

		for ( let n = 0; n < this.cascades; n ++ ) {

			const { capacity, slotOffset } = cascades[ n ];

			for ( let i = 0; i < capacity; i ++ ) freeList[ slotOffset + i ] = i;

			counterArray[ FREED * this.cascades + n ] = capacity;
			counterArray[ counters + FREED * this.cascades + n ] = capacity;

		}

		this._table = make( new Uint32Array( tables * 2 ).fill( EMPTY ), 'uint', 1, true );
		this._slotOf = make( new Uint32Array( tables * 2 ).fill( NONE ), 'uint', 1 );
		this._counters = make( counterArray, 'uint', 1, true );
		this._freeList = make( freeList, 'uint', 1 );

		// per slot: its cell and level, the frames it was allocated and last alive, the frame it lives until,
		// its ray count, the rays deposited this frame, and the rays of the R2 sequence of the probe

		this._keys = make( new Int32Array( slots * 4 ), 'ivec4', 4 );
		this._born = make( new Uint32Array( slots ), 'uint', 1 );
		this._alive = make( new Uint32Array( slots ), 'uint', 1 );
		this._expiry = make( new Uint32Array( slots ), 'uint', 1, true );
		this._quality = make( new Float32Array( slots ), 'float', 1 );
		this._deposited = make( new Uint32Array( slots ), 'uint', 1, true );
		this._rayCount = make( new Uint32Array( slots ), 'uint', 1, true );
		this._rayOffset = make( new Uint32Array( slots ), 'uint', 1 );
		this._rayIndex = make( new Uint32Array( cascades[ 0 ].capacity ), 'uint', 1, true );

		// per bin: the deposits of the frame, the weighted fluence and the weights

		this._deposits = make( new Uint32Array( bins * WORDS ), 'uint', 1, true );
		this._fluence = make( new Float32Array( bins * 4 ), 'vec4', 4 );
		this._weights = make( new Float32Array( bins ), 'float', 1 );
		this._merged = make( new Float32Array( merged * 4 ), 'vec4', 4 );
		this._irradiance = make( new Float32Array( cascades[ 0 ].capacity * IRRADIANCE_TEXELS * 4 ), 'vec4', 4 );

		this._buffers = [ this._table, this._slotOf, this._counters, this._freeList, this._keys, this._born, this._alive, this._expiry, this._quality, this._deposited, this._rayCount, this._rayOffset, this._rayIndex, this._deposits, this._fluence, this._weights, this._merged, this._irradiance ];
		this.memory = this._buffers.reduce( ( total, buffer ) => total + buffer.value.array.byteLength, 0 );

	}

	/**
	 * Returns the G-buffer, created on first use.
	 *
	 * @private
	 */
	_getGBuffer( width, height ) {

		if ( this._gbuffer !== null ) return this._gbuffer;

		const gbuffer = new RenderTarget( width, height, { count: 4, type: HalfFloatType } );
		gbuffer.textures[ 0 ].name = 'output';
		gbuffer.textures[ 1 ].name = 'albedo';
		gbuffer.textures[ 2 ].name = 'normal';
		gbuffer.textures[ 3 ].name = 'velocity';
		gbuffer.depthTexture = new DepthTexture( width, height, FloatType );

		// the vertex normal on the side of the visible face, the flat normal of the triangle towards the camera: the back
		// faces of double sided materials and normals against the winding are flipped, but not smooth normals that point
		// a little away from the camera at grazing angles

		const flat = cross( positionWorld.dFdx(), positionWorld.dFdy() );
		const facing = select( dot( flat, cameraPosition.sub( positionWorld ) ).lessThan( 0 ), flat.negate(), flat );
		const normal = select( dot( normalWorldGeometry, facing ).lessThan( 0 ), normalWorldGeometry.negate(), normalWorldGeometry );

		this._gbuffer = gbuffer;
		this._mrt = mrt( { output, albedo: vec4( diffuseColor.rgb, 1 ), normal: vec4( normal, 1 ), velocity } );

		return gbuffer;

	}

	/**
	 * Renders the sky of the scene background into an equirectangular texture, which the rays sample:
	 * compute shaders can not sample a PMREM, whose cube texture defaults to the reflection vector of the camera.
	 *
	 * @private
	 */
	_createSky( renderer ) {

		this._skyTarget?.dispose();
		this._skyTarget = null;

		const radiosity = this._radiosity;

		if ( radiosity._environment === null ) return;

		const target = new RenderTarget( 256, 128, { type: HalfFloatType, depthBuffer: false } );
		const material = new NodeMaterial();

		material.fragmentNode = Fn( () => {

			const phi = uv().x.sub( 0.5 ).mul( 2 * Math.PI );
			const theta = uv().y.sub( 0.5 ).mul( Math.PI );
			const direction = vec3( cos( theta ).mul( cos( phi ) ), sin( theta ), cos( theta ).mul( sin( phi ) ) );

			return vec4( radiosity._skyRadiance( direction ).div( Math.PI ), 1 );

		} )();

		const quad = new QuadMesh( material );
		const previousTarget = renderer.getRenderTarget();

		renderer.setRenderTarget( target );
		quad.render( renderer );
		renderer.setRenderTarget( previousTarget );

		material.dispose();

		this._skyTarget = target;

	}

	/**
	 * Copies the first visible directional light into the sun uniforms.
	 *
	 * @private
	 */
	_updateSun( scene ) {

		let sun = null;

		scene.traverseVisible( ( object ) => {

			if ( sun === null && object.isDirectionalLight === true ) sun = object;

		} );

		if ( sun === null ) {

			this._sunColor.value.set( 0, 0, 0 );
			return;

		}

		sun.updateMatrixWorld();
		sun.target.updateMatrixWorld();

		this._sunDirection.value.setFromMatrixPosition( sun.matrixWorld ).sub( _position.setFromMatrixPosition( sun.target.matrixWorld ) ).normalize();
		this._sunColor.value.set( sun.color.r, sun.color.g, sun.color.b ).multiplyScalar( sun.intensity );

	}

	// --- shader helpers ---

	/**
	 * The world position of a screen coordinate and depth.
	 *
	 * @private
	 */
	_worldPosition( uv, depth ) {

		const view = getViewPosition( uv, depth, this._projectionInverse );

		return this._cameraWorld.mul( vec4( view, 1 ) ).xyz;

	}

	/**
	 * The normal of a pixel of the G-buffer, which is on the side of the visible face. Flipping it towards the camera
	 * instead would turn smooth normals that point a little away from it at grazing angles into the surface, whose
	 * pixels would then gather the light behind it.
	 *
	 * @private
	 */
	_facingNormal( normal ) {

		return normalize( normal );

	}

	/**
	 * The surface of a pixel of the G-buffer.
	 *
	 * @private
	 */
	_pixelSurface( pixel ) {

		const depth = texture( this._gbuffer.depthTexture ).load( pixel ).x.toVar();
		const uv = vec2( pixel ).add( 0.5 ).div( this._resolution.xy );
		const position = this._worldPosition( uv, depth ).toVar();
		const normal = this._facingNormal( texture( this._gbuffer.textures[ 2 ] ).load( pixel ).xyz ).toVar();

		return { valid: depth.lessThan( 1 ), position, normal };

	}

	/**
	 * The continuous level of detail of a position: the octaves of its Chebyshev distance to the camera over
	 * `levelDistance`.
	 *
	 * @private
	 */
	_levelIndex( position ) {

		const offset = abs( position.sub( this._cameraPosition ) ).toVar();

		return log2( max( offset.x, max( offset.y, offset.z ) ).div( this.levelDistance ).max( 1e-6 ) ).add( this._lodBias );

	}

	/**
	 * The level of detail of a position, or -1 beyond the last: `estimate_lod()` in chandra.
	 *
	 * @private
	 */
	_levelOf( position ) {

		const level = int( ceil( this._levelIndex( position ).max( 0 ) ) ).toVar();

		return select( level.greaterThanEqual( this.levels ), int( - 1 ), level );

	}

	/**
	 * The levels of detail of a position and their weights: near the boundary of the next level both, see
	 * `blendingRadius` (`iter_lods()` in chandra). The second weight is zero if there is one level. The first
	 * level is -1 beyond the last.
	 *
	 * @private
	 */
	_levelsOf( position ) {

		const index = this._levelIndex( position ).max( - 0.9 ).toVar();
		const lower = int( ceil( index ) ).toVar();
		const valid = lower.greaterThanEqual( 0 ).and( lower.lessThan( this.levels ) ).toVar();

		// the weight of the finer level falls from 1 to 0 over the last log2( blendingRadius ) of its octave of the
		// distance

		const finer = float( 1 ).sub( index.sub( float( lower ).sub( 1 ) ) ).div( this._blendingOctaves.max( 1e-3 ) ).clamp( 0, 1 ).toVar();

		return [
			{ level: lower, weight: select( valid, finer, float( 0 ) ) },
			{ level: lower.add( 1 ), weight: select( valid.and( lower.add( 1 ).lessThan( this.levels ) ), finer.oneMinus(), float( 0 ) ) }
		];

	}

	/**
	 * The level of detail of a position with the larger weight, or -1 beyond the last, which always has probes:
	 * the pixels count and trace their rays in its probes (`estimate_lod()` in chandra, whose finer level always
	 * has some weight).
	 *
	 * @private
	 */
	_primaryLevel( position ) {

		const [ first, second ] = this._levelsOf( position );

		return select( first.weight.add( second.weight ).greaterThan( 0 ), select( first.weight.greaterThanEqual( second.weight ), first.level, second.level ), int( - 1 ) );

	}

	/**
	 * The probe spacing of a cascade in a level of detail.
	 *
	 * @private
	 */
	_spacingOf( level, n ) {

		return exp2( float( level ).add( n ) ).mul( this.spacing );

	}

	/**
	 * The cell of a position in a cascade and level of detail.
	 *
	 * @private
	 */
	_cellOf( position, level, n ) {

		return ivec3( floor( position.div( this._spacingOf( level, n ) ) ) );

	}

	/**
	 * The cell of a surface in a cascade and level of detail, from a point in front of it, see `KEY_OFFSET`.
	 *
	 * @private
	 */
	_surfaceCell( position, normal, level, n ) {

		return this._cellOf( position.add( normal.mul( this._spacingOf( level, 0 ).mul( KEY_OFFSET ) ) ), level, n );

	}

	/**
	 * The orientation of a normal: its dominant axis times two, plus one for the negative side, see
	 * `orientations`.
	 *
	 * @private
	 */
	_groupOf( normal ) {

		if ( this.orientations === false ) return uint( 0 );

		const a = abs( normal ).toVar();
		const axis = select( a.x.greaterThanEqual( a.y ).and( a.x.greaterThanEqual( a.z ) ), uint( 0 ), select( a.y.greaterThanEqual( a.z ), uint( 1 ), uint( 2 ) ) ).toVar();
		const component = select( axis.equal( 0 ), normal.x, select( axis.equal( 1 ), normal.y, normal.z ) );

		return axis.mul( 2 ).add( select( component.lessThan( 0 ), uint( 1 ), uint( 0 ) ) );

	}

	/**
	 * The key of a cell in a level of detail: its coordinates modulo 512, which are unique within a level,
	 * and the level.
	 *
	 * @private
	 */
	_keyOf( cell, level ) {

		const c = uint( cell.x ).bitAnd( 511 ).bitOr( uint( cell.y ).bitAnd( 511 ).shiftLeft( 9 ) ).bitOr( uint( cell.z ).bitAnd( 511 ).shiftLeft( 18 ) );

		return c.bitOr( uint( level ).shiftLeft( 27 ) );

	}

	/**
	 * The cell and level of detail of a key in a cascade: the nearest cell to the camera with its coordinates
	 * modulo 512.
	 *
	 * @private
	 */
	_decodeKey( key, n ) {

		const level = int( key.shiftRight( 27 ) ).toVar();
		const wrapped = ivec3( int( key.bitAnd( 511 ) ), int( key.shiftRight( 9 ).bitAnd( 511 ) ), int( key.shiftRight( 18 ).bitAnd( 511 ) ) );
		const camera = this._cellOf( this._cameraPosition, level, n ).toVar();
		const delta = wrapped.sub( camera ).bitAnd( ivec3( 511 ) ).toVar();

		return { cell: camera.add( select( delta.greaterThanEqual( ivec3( 256 ) ), delta.sub( 512 ), delta ) ), level };

	}

	/**
	 * A counter of the current or last frame.
	 *
	 * @private
	 */
	_counter( index, previous = false ) {

		const half = previous ? uint( 1 ).sub( this._parity ) : this._parity;

		return this._counters.element( half.mul( this._layout.counters ).add( index ) );

	}

	/**
	 * Whether a slot of a cascade holds a probe of this frame.
	 *
	 * @private
	 */
	_isAlive( n, slot ) {

		return this._alive.element( slot.add( this._layout.cascades[ n ].slotOffset ) ).equal( this._frameNode );

	}

	/**
	 * Inserts a key into the current table of a cascade and orientation.
	 *
	 * @private
	 */
	_insert( n, key, group ) {

		const cascade = this._layout.cascades[ n ];
		const base = this._parity.mul( this._layout.tables ).add( cascade.tableOffset ).add( uint( group ).mul( cascade.tableSize ) );

		this._insertInto( this._table, base, cascade.tableSize, key );

	}

	/**
	 * Inserts a key into a hash table at an offset: lock-free, an insertion that meets a larger key takes its
	 * slot and inserts that key further along.
	 *
	 * @private
	 */
	_insertInto( table, offset, size, key ) {

		const mask = size - 1;
		const base = offset.toVar();
		const item = key.toVar();
		const slot = mixKey( item ).bitAnd( mask ).toVar();

		Loop( MAX_PROBES, () => {

			const previous = atomicMin( table.element( base.add( slot ) ), item ).toVar();

			If( previous.equal( EMPTY ).or( previous.equal( item ) ), () => {

				Break();

			} );

			If( previous.greaterThan( item ), () => {

				item.assign( previous );

			} );

			slot.assign( slot.add( 1 ).bitAnd( mask ) );

		} );

	}

	/**
	 * Returns the slot of a key in the current or last table of a cascade and orientation, or `NONE`.
	 *
	 * @private
	 */
	_lookup( n, key, group, previous = false ) {

		const cascade = this._layout.cascades[ n ];
		const mask = cascade.tableSize - 1;

		return Fn( () => {

			const half = previous ? uint( 1 ).sub( this._parity ) : this._parity;
			const base = half.mul( this._layout.tables ).add( cascade.tableOffset ).add( uint( group ).mul( cascade.tableSize ) ).toVar();
			const slot = mixKey( key ).bitAnd( mask ).toVar();
			const probe = uint( NONE ).toVar();

			Loop( MAX_PROBES, () => {

				const entry = atomicLoad( this._table.element( base.add( slot ) ) ).toVar();

				If( entry.equal( key ), () => {

					probe.assign( this._slotOf.element( base.add( slot ) ) );
					Break();

				} );

				If( entry.equal( EMPTY ), () => {

					Break();

				} );

				slot.assign( slot.add( 1 ).bitAnd( mask ) );

			} );

			return probe;

		} )();

	}

	/**
	 * The bin of a direction in a cascade, by its equal-area coordinates (`SphericalEncoder` in chandra).
	 *
	 * @private
	 */
	_binOf( direction, n ) {

		const theta = this._layout.cascades[ n ].theta;
		const u = fract( atan( direction.y, direction.x ).div( 2 * Math.PI ) );
		const v = direction.z.mul( 0.5 ).add( 0.5 ).clamp( 0, 0.9999999 );
		const i = uint( u.mul( 2 * theta ) ).min( 2 * theta - 1 );
		const j = uint( v.mul( theta ) ).min( theta - 1 );

		return j.mul( 2 * theta ).add( i );

	}

	/**
	 * The average radiance over the cosine lobe of a normal at a position: the octahedral irradiance maps of the
	 * probes of the finest cascade around it, weighted by their trilinear weights, the levels of detail and their
	 * ray counts (`shade_optimized_32()` in chandra).
	 *
	 * @private
	 */
	_gather( position, normal ) {

		const sum = vec4( 0 ).toVar();

		// the texels of the normal in the octahedral maps

		const texel = octahedralEncode( normal ).mul( IRRADIANCE_SIZE ).sub( 0.5 ).toVar();
		const lowerTexel = ivec2( floor( texel ) ).toVar();
		const t = texel.sub( vec2( lowerTexel ) ).toVar();
		const texels = [];

		for ( let corner = 0; corner < 4; corner ++ ) {

			const index = octahedralTexel( lowerTexel.add( ivec2( corner & 1, corner >> 1 ) ) );
			const weight = ( corner & 1 ? t.x : t.x.oneMinus() ).mul( corner & 2 ? t.y : t.y.oneMinus() );

			texels.push( { index: index.toVar(), weight: weight.toVar() } );

		}

		const group = this._groupOf( normal ).toVar();

		for ( const { level, weight: levelWeight } of this._levelsOf( position ) ) {

			If( levelWeight.greaterThan( 0 ), () => {

				const spacing = this._spacingOf( level, 0 ).toVar();
				const point = position.add( normal.mul( spacing.mul( KEY_OFFSET ) ) ).toVar();
				const lower = ivec3( floor( point.sub( spacing.mul( 0.5 ) ).div( spacing ) ) ).toVar();
				const f = fract( point.div( spacing ).sub( 0.5 ) ).toVar();

				Loop( 8, ( { i: corner } ) => {

					const offset = ivec3( corner.bitAnd( 1 ), corner.shiftRight( 1 ).bitAnd( 1 ), corner.shiftRight( 2 ).bitAnd( 1 ) ).toVar();
					const slot = this._lookup( 0, this._keyOf( lower.add( offset ), level ), group ).toVar();

					If( slot.notEqual( NONE ), () => {

						const w = select( offset.equal( ivec3( 1 ) ), f, f.oneMinus() );
						const quality = this.useQualityWeight ? this._quality.element( slot ) : float( 1 );
						const weight = w.x.mul( w.y ).mul( w.z ).mul( levelWeight ).mul( quality ).toVar();
						const base = slot.mul( IRRADIANCE_TEXELS ).toVar();

						for ( const { index, weight: texelWeight } of texels ) {

							sum.addAssign( this._irradiance.element( base.add( index ) ).mul( texelWeight.mul( weight ) ) );

						}

					} );

				} );

			} );

		}

		return sum.rgb.div( sum.w.max( 1e-5 ) );

	}

	/**
	 * The average radiance over the cosine lobe of a normal at a hit, from the merged radiance of the last frame
	 * of its secondary probe, two levels coarser (`sample_secondary()` in chandra).
	 *
	 * @private
	 */
	_sampleSecondary( position, normal ) {

		const first = this._layout.cascades[ 0 ];
		const sum = vec4( 0 ).toVar();
		const level = this._levelOf( position ).add( 2 ).toVar();

		If( level.greaterThanEqual( 2 ).and( level.lessThan( this.levels ) ), () => {

			const slot = this._lookup( 0, this._keyOf( this._surfaceCell( position, normal, level, 0 ), level ), this._groupOf( normal ) ).toVar();

			// a probe allocated this frame has no light yet

			If( slot.notEqual( NONE ).and( this._born.element( slot ).notEqual( this._frameNode ) ), () => {

				Loop( { start: 0, end: first.bins, type: 'uint', condition: '<', name: 'bin' }, ( { bin } ) => {

					const weight = dot( binDirection( bin, first.theta ), normal ).toVar();
					const radiance = this._merged.element( slot.mul( first.bins ).add( bin ) ).toVar();

					If( weight.greaterThan( 0 ).and( radiance.w.greaterThan( 0 ) ), () => {

						sum.addAssign( vec4( radiance.rgb, 1 ).mul( weight ) );

					} );

				} );

			} );

		} );

		return sum.rgb.div( sum.w.max( 1e-5 ) );

	}

	// --- passes ---

	/**
	 * Creates the passes that do not depend on the resolution.
	 *
	 * @private
	 */
	_createKernels() {

		const layout = this._layout;
		const cascades = layout.cascades;
		const first = cascades[ 0 ];

		// clears the current table and node set and the counters of the frame

		const clear = Fn( () => {

			If( instanceIndex.lessThan( layout.tables ), () => {

				atomicStore( this._table.element( this._parity.mul( layout.tables ).add( instanceIndex ) ), uint( EMPTY ) );

			} );

			If( instanceIndex.lessThan( layout.counters ), () => {

				atomicStore( this._counter( instanceIndex ), uint( 0 ) );

			} );

		} )().compute( layout.tables ).setName( 'RC.Clear' );

		// the probes of the finest cascade of the last frame that still live, e.g. the secondary ones

		const keep = Fn( () => {

			const slot = instanceIndex;

			If( this._alive.element( slot ).equal( this._frameNode.sub( 1 ) ).and( atomicLoad( this._expiry.element( slot ) ).greaterThanEqual( this._frameNode ) ), () => {

				const key = this._keys.element( slot ).toVar();

				this._insert( 0, this._keyOf( key.xyz, key.w.bitAnd( 15 ) ), uint( key.w.shiftRight( 4 ) ) );

			} );

		} )().compute( first.capacity ).setName( 'RC.Keep' );

		const assign = [], fill = [], free = [], countUp = [], offsets = [], accumulate = [], merge = [];

		for ( let n = 0; n < this.cascades; n ++ ) {

			assign.push( this._createAssign( n ) );
			fill.push( n > 0 ? this._createFill( n ) : null );
			free.push( this._createFree( n ) );
			countUp.push( n > 0 ? this._createCountUp( n ) : null );
			offsets.push( this._createOffsets( n ) );
			accumulate.push( this._createAccumulate( n ) );
			merge.push( this._createMerge( n ) );

		}

		// the octahedral irradiance maps of the probes of the finest cascade: per texel, the merged radiance of the
		// bins weighted by their cosine to the texel's direction, and the sum of the weights

		const irradiance = Fn( () => {

			const slot = instanceIndex.div( IRRADIANCE_TEXELS ).toVar();
			const texel = instanceIndex.mod( IRRADIANCE_TEXELS ).toVar();

			If( this._isAlive( 0, slot ), () => {

				const direction = octahedralDecode( vec2( float( texel.mod( IRRADIANCE_SIZE ) ), float( texel.div( IRRADIANCE_SIZE ) ) ).add( 0.5 ).div( IRRADIANCE_SIZE ) ).toVar();
				const sum = vec4( 0 ).toVar();

				Loop( { start: 0, end: first.bins, type: 'uint', condition: '<', name: 'bin' }, ( { bin } ) => {

					const radiance = this._merged.element( slot.mul( first.bins ).add( bin ) ).toVar();

					If( radiance.w.greaterThan( 0 ), () => {

						sum.addAssign( vec4( radiance.rgb, 1 ).mul( dot( direction, binDirection( bin, first.theta ) ).max( 0 ) ) );

					} );

				} );

				this._irradiance.element( instanceIndex ).assign( sum );

			} );

		} )().compute( first.capacity * IRRADIANCE_TEXELS ).setName( 'RC.Irradiance' );

		return { clear, keep, assign, fill, free, countUp, offsets, accumulate, merge, irradiance };

	}

	/**
	 * Creates the pass that gives the keys of the current table of a cascade their slots: the slot of the last
	 * frame for keys that were in its table, a free slot for new ones.
	 *
	 * @private
	 */
	_createAssign( n ) {

		const cascade = this._layout.cascades[ n ];
		const groups = this._layout.groups;

		return Fn( () => {

			const bucket = instanceIndex;
			const group = bucket.div( cascade.tableSize ).toVar();

			If( bucket.lessThan( cascade.tableSize * groups ), () => {

				const index = this._parity.mul( this._layout.tables ).add( cascade.tableOffset ).add( bucket ).toVar();
				const key = atomicLoad( this._table.element( index ) ).toVar();

				this._slotOf.element( index ).assign( uint( NONE ) );

				If( key.notEqual( EMPTY ), () => {

					const slot = this._lookup( n, key, group, true ).toVar();

					If( slot.equal( NONE ), () => {

						const allocation = atomicAdd( this._counter( ALLOCATED * this.cascades + n ), uint( 1 ) );

						If( allocation.lessThan( atomicLoad( this._counter( FREED * this.cascades + n, true ) ) ), () => {

							slot.assign( this._freeList.element( allocation.add( cascade.slotOffset ) ) );

							const global = slot.add( cascade.slotOffset );

							this._born.element( global ).assign( this._frameNode );
							this._quality.element( global ).assign( 0 );
							atomicStore( this._expiry.element( global ), uint( 0 ) );

							if ( n === 0 ) atomicAdd( this._counter( 3 * this.cascades ), uint( 1 ) );

						} );

					} );

					If( slot.notEqual( NONE ), () => {

						const global = slot.add( cascade.slotOffset ).toVar();
						const decoded = this._decodeKey( key, n );

						this._slotOf.element( index ).assign( slot );
						this._keys.element( global ).assign( ivec4( decoded.cell, decoded.level.bitOr( int( group ).shiftLeft( 4 ) ) ) );
						this._alive.element( global ).assign( this._frameNode );

						atomicAdd( this._counter( PROBES * this.cascades + n ), uint( 1 ) );

					} );

				} );

			} );

		} )().compute( cascade.tableSize * groups ).setName( 'RC.Assign' + n );

	}

	/**
	 * Creates the pass that inserts the probes of a cascade: the cells that hold the probes of the finer one.
	 *
	 * @private
	 */
	_createFill( n ) {

		const finer = this._layout.cascades[ n - 1 ];

		return Fn( () => {

			const slot = instanceIndex;

			If( this._isAlive( n - 1, slot ), () => {

				const key = this._keys.element( slot.add( finer.slotOffset ) ).toVar();

				this._insert( n, this._keyOf( ivec3( floor( vec3( key.xyz ).mul( 0.5 ) ) ), key.w.bitAnd( 15 ) ), uint( key.w.shiftRight( 4 ) ) );

			} );

		} )().compute( finer.capacity ).setName( 'RC.Fill' + n );

	}

	/**
	 * Creates the pass that lists the slots of a cascade without a probe this frame, for the next frame, and
	 * resets the ray counts of the slots.
	 *
	 * @private
	 */
	_createFree( n ) {

		const cascade = this._layout.cascades[ n ];

		return Fn( () => {

			const slot = instanceIndex;
			const global = slot.add( cascade.slotOffset ).toVar();

			atomicStore( this._rayCount.element( global ), uint( 0 ) );

			if ( n === 0 ) atomicStore( this._rayIndex.element( slot ), uint( 0 ) );

			If( this._isAlive( n, slot ).not(), () => {

				const index = atomicAdd( this._counter( FREED * this.cascades + n ), uint( 1 ) );

				this._freeList.element( index.add( cascade.slotOffset ) ).assign( slot );

			} );

		} )().compute( cascade.capacity ).setName( 'RC.Free' + n );

	}

	/**
	 * Creates the pass that adds the rays of the probes of a cascade to the next coarser one.
	 *
	 * @private
	 */
	_createCountUp( n ) {

		const finer = this._layout.cascades[ n - 1 ];
		const cascade = this._layout.cascades[ n ];

		return Fn( () => {

			const slot = instanceIndex;

			If( this._isAlive( n - 1, slot ), () => {

				const global = slot.add( finer.slotOffset ).toVar();
				const rays = atomicLoad( this._rayCount.element( global ) ).toVar();

				If( rays.greaterThan( 0 ), () => {

					const key = this._keys.element( global ).toVar();
					const parent = this._lookup( n, this._keyOf( ivec3( floor( vec3( key.xyz ).mul( 0.5 ) ) ), key.w.bitAnd( 15 ) ), uint( key.w.shiftRight( 4 ) ) ).toVar();

					If( parent.notEqual( NONE ), () => {

						atomicAdd( this._rayCount.element( parent.add( cascade.slotOffset ) ), rays );

					} );

				} );

			} );

		} )().compute( finer.capacity ).setName( 'RC.CountUp' + n );

	}

	/**
	 * Creates the pass that gives the probes of a cascade their contiguous ranges of the R2 sequence: the
	 * coarsest ones from a counter, the finer ones split the range of the coarser probe around them
	 * (`compute_ray_offset()` in chandra).
	 *
	 * @private
	 */
	_createOffsets( n ) {

		const cascades = this._layout.cascades;
		const last = n === this.cascades - 1;
		const cascade = last ? cascades[ n ] : cascades[ n + 1 ];

		return Fn( () => {

			const slot = instanceIndex;

			if ( last ) {

				If( this._isAlive( n, slot ), () => {

					const global = slot.add( cascade.slotOffset ).toVar();
					const rays = atomicLoad( this._rayCount.element( global ) );

					this._rayOffset.element( global ).assign( atomicAdd( this._counter( 3 * this.cascades + 1 ), rays ) );

				} );

			} else {

				const finer = cascades[ n ];

				If( this._isAlive( n + 1, slot ), () => {

					const global = slot.add( cascade.slotOffset ).toVar();
					const key = this._keys.element( global ).toVar();
					const offset = this._rayOffset.element( global ).toVar();

					Loop( 8, ( { i } ) => {

						const corner = ivec3( i.bitAnd( 1 ), i.shiftRight( 1 ).bitAnd( 1 ), i.shiftRight( 2 ).bitAnd( 1 ) );
						const child = this._lookup( n, this._keyOf( key.xyz.mul( 2 ).add( corner ), key.w.bitAnd( 15 ) ), uint( key.w.shiftRight( 4 ) ) ).toVar();

						If( child.notEqual( NONE ), () => {

							const childGlobal = child.add( finer.slotOffset ).toVar();

							this._rayOffset.element( childGlobal ).assign( offset );
							offset.addAssign( atomicLoad( this._rayCount.element( childGlobal ) ) );

						} );

					} );

				} );

			}

		} )().compute( cascade.capacity ).setName( 'RC.Offsets' + n );

	}

	/**
	 * Creates the pass that adds the rays of a frame to the bins of a cascade, after decaying their sums, and
	 * updates the ray counts of the probes.
	 *
	 * @private
	 */
	_createAccumulate( n ) {

		const cascade = this._layout.cascades[ n ];

		return Fn( () => {

			const item = instanceIndex;
			const slot = item.div( cascade.bins ).toVar();
			const bin = item.mod( cascade.bins ).toVar();

			If( this._isAlive( n, slot ), () => {

				const global = slot.add( cascade.slotOffset ).toVar();
				const index = item.add( cascade.binOffset ).toVar();
				const base = index.mul( WORDS ).toVar();
				const born = this._born.element( global ).equal( this._frameNode ).toVar();
				const fluence = vec4( 0 ).toVar();
				const weight = float( 0 ).toVar();

				// the decay of the last frame's sums, which chandra applies after the merge

				If( born.not(), () => {

					fluence.assign( this._fluence.element( index ) );
					weight.assign( this._weights.element( index ) );

					If( weight.greaterThan( this._minDirectionWeight ), () => {

						fluence.mulAssign( this._qualityDecay );
						weight.mulAssign( this._qualityDecay );

					} );

				} );

				const rays = atomicLoad( this._deposits.element( base.add( 4 ) ) ).min( MAX_RAYS ).toVar();

				If( rays.greaterThan( 0 ), () => {

					const word = ( k ) => float( atomicLoad( this._deposits.element( base.add( k ) ) ) );

					fluence.addAssign( vec4( vec3( word( 0 ), word( 1 ), word( 2 ) ).div( SCALE ), word( 3 ) ) );
					weight.addAssign( float( rays ) );

					for ( let k = 0; k < WORDS; k ++ ) atomicStore( this._deposits.element( base.add( k ) ), uint( 0 ) );

				} );

				this._fluence.element( index ).assign( fluence );
				this._weights.element( index ).assign( weight );

				If( bin.equal( 0 ), () => {

					const quality = select( born, float( 0 ), this._quality.element( global ).mul( this._qualityDecay ) );

					this._quality.element( global ).assign( quality.add( float( atomicLoad( this._deposited.element( global ) ) ) ) );
					atomicStore( this._deposited.element( global ), uint( 0 ) );

				} );

			} );

		} )().compute( cascade.capacity * cascade.bins ).setName( 'RC.Accumulate' + n );

	}

	/**
	 * Creates the pass that merges a cascade with the next coarser one: the mean radiance of the interval of a
	 * bin, plus the light beyond it times its mean transmittance, interpolated from the 8 coarser probes around
	 * weighted by their ray counts. Bins without rays have no light. The coarser cascades store the mean of the
	 * bins with light within each bin of the next finer one (`merge_cache_raw()` in chandra).
	 *
	 * @private
	 */
	_createMerge( n ) {

		const layout = this._layout;
		const cascade = layout.cascades[ n ];
		const coarser = layout.cascades[ n + 1 ] ?? null;
		const theta = cascade.theta;

		return Fn( () => {

			const item = instanceIndex;
			const slot = item.div( cascade.outputs ).toVar();
			const output = item.mod( cascade.outputs ).toVar();

			If( this._isAlive( n, slot ), () => {

				const key = this._keys.element( slot.add( cascade.slotOffset ) ).toVar();
				const corners = [];

				if ( coarser !== null ) {

					// the coarser cells around this one lie a quarter and three quarters of a cell away

					const cell = key.xyz.toVar();
					const lower = ivec3( floor( vec3( cell.sub( 1 ) ).mul( 0.5 ) ) ).toVar();
					const lowerWeight = select( cell.bitAnd( ivec3( 1 ) ).equal( ivec3( 0 ) ), vec3( 0.25 ), vec3( 0.75 ) ).toVar();

					for ( let corner = 0; corner < 8; corner ++ ) {

						const offset = ivec3( corner & 1, ( corner >> 1 ) & 1, ( corner >> 2 ) & 1 );
						const probe = this._lookup( n + 1, this._keyOf( lower.add( offset ), key.w.bitAnd( 15 ) ), uint( key.w.shiftRight( 4 ) ) ).toVar();
						const wx = ( corner & 1 ) ? lowerWeight.x.oneMinus() : lowerWeight.x;
						const wy = ( corner & 2 ) ? lowerWeight.y.oneMinus() : lowerWeight.y;
						const wz = ( corner & 4 ) ? lowerWeight.z.oneMinus() : lowerWeight.z;
						const weight = wx.mul( wy ).mul( wz ).toVar();

						if ( this.useQualityWeight ) {

							If( probe.notEqual( NONE ), () => {

								weight.mulAssign( this._quality.element( probe.add( coarser.slotOffset ) ) );

							} );

						}

						corners.push( { probe, weight } );

					}

				}

				const lightOf = ( bin ) => {

					const index = slot.mul( cascade.bins ).add( bin ).add( cascade.binOffset ).toVar();
					const weight = this._weights.element( index ).toVar();
					const fluence = this._fluence.element( index ).div( weight.max( 1e-4 ) ).toVar();
					const radiance = fluence.rgb.toVar();

					if ( coarser !== null ) {

						const sum = vec4( 0 ).toVar();

						for ( const { probe, weight: probeWeight } of corners ) {

							If( probe.notEqual( NONE ), () => {

								const value = this._merged.element( probe.mul( coarser.outputs ).add( bin ).add( coarser.mergedOffset ) ).toVar();

								If( value.w.greaterThan( 0 ), () => {

									sum.addAssign( vec4( value.rgb, 1 ).mul( probeWeight ) );

								} );

							} );

						}

						radiance.addAssign( sum.rgb.div( sum.w.max( 1e-5 ) ).mul( fluence.a ) );

					}

					return { radiance, valid: weight.greaterThanEqual( 1e-4 ) };

				};

				const target = this._merged.element( item.add( cascade.mergedOffset ) );

				if ( n === 0 ) {

					const { radiance, valid } = lightOf( output );

					target.assign( select( valid, vec4( radiance, 1 ), vec4( 0 ) ) );

				} else {

					const column = output.mod( theta ).mul( 2 ).toVar();
					const row = output.div( theta ).mul( 2 ).toVar();
					const sum = vec4( 0 ).toVar();

					for ( let child = 0; child < 4; child ++ ) {

						const bin = row.add( child >> 1 ).mul( 2 * theta ).add( column.add( child & 1 ) );
						const { radiance, valid } = lightOf( bin );

						If( valid, () => {

							sum.addAssign( vec4( radiance, 1 ) );

						} );

					}

					target.assign( select( sum.w.greaterThan( 0 ), vec4( sum.rgb.div( sum.w ), 1 ), vec4( 0 ) ) );

				}

			} );

		} )().compute( cascade.capacity * cascade.outputs ).setName( 'RC.Merge' + n );

	}

	/**
	 * Creates the passes that depend on the resolution: the probes of the pixels and of the hits, the rays of
	 * the probes, and the rays.
	 *
	 * @private
	 */
	_createFrameKernels() {

		const capacity = this._hitCapacity;
		const width = this._screenWidth, height = this._screenHeight;
		const pixels = width.mul( height );

		// per pixel, the hit of its ray: the position and the frame, and the normal

		this._hits = storage( new StorageBufferAttribute( new Float32Array( capacity * 8 ), 4 ), 'vec4', capacity * 2 );

		const pixelOf = ( index ) => ivec2( int( index.mod( width ) ), int( index.div( width ) ) );

		// every pixel inserts the probes of its levels of detail

		const gather = Fn( () => {

			If( instanceIndex.lessThan( pixels ), () => {

				const surface = this._pixelSurface( pixelOf( instanceIndex ) );

				If( surface.valid, () => {

					const group = this._groupOf( surface.normal ).toVar();

					for ( const { level, weight } of this._levelsOf( surface.position ) ) {

						If( weight.greaterThan( 0 ), () => {

							this._insert( 0, this._keyOf( this._surfaceCell( surface.position, surface.normal, level, 0 ), level ), group );

						} );

					}

				} );

			} );

		} )().computeKernel().setName( 'RC.Gather' );

		// the hits of the last frame insert their secondary probes, two levels coarser

		const secondaryHit = ( index ) => {

			const position = this._hits.element( index.mul( 2 ) ).toVar();
			const valid = position.w.equal( float( this._frameNode.sub( 1 ) ) ).and( this._frameNode.greaterThan( 1 ) );
			const level = this._levelOf( position.xyz ).add( 2 ).toVar();

			return { valid: valid.and( level.greaterThanEqual( 2 ) ).and( level.lessThan( this.levels ) ), position: position.xyz, normal: this._hits.element( index.mul( 2 ).add( 1 ) ).xyz, level };

		};

		const gatherSecondary = this.secondary ? Fn( () => {

			If( instanceIndex.lessThan( pixels ), () => {

				const hit = secondaryHit( instanceIndex );

				If( hit.valid, () => {

					this._insert( 0, this._keyOf( this._surfaceCell( hit.position, hit.normal, hit.level, 0 ), hit.level ), this._groupOf( hit.normal ) );

				} );

			} );

		} )().computeKernel().setName( 'RC.GatherSecondary' ) : null;

		// a pixel of each block of rayStride x rayStride pixels traces a ray, in turns

		const stride = this.rayStride;
		const blocksX = this._blocksX, blocks = this._blocks;

		const tracedPixel = ( index ) => {

			const block = ivec2( int( index.mod( blocksX ) ), int( index.div( blocksX ) ) );
			const turn = this._frameNode.add( uint( hash( index ).mul( stride * stride ) ) ).mod( stride * stride );
			const pixel = block.mul( stride ).add( ivec2( int( turn.mod( stride ) ), int( turn.div( stride ) ) ) ).toVar();

			return { pixel, valid: index.lessThan( blocks ).and( pixel.x.lessThan( int( width ) ) ).and( pixel.y.lessThan( int( height ) ) ) };

		};

		// the rays of the probes of the finest cascade, from their pixels in their level of detail

		const count = Fn( () => {

			const { pixel, valid } = tracedPixel( instanceIndex );

			If( valid, () => {

				const surface = this._pixelSurface( pixel );
				const level = this._primaryLevel( surface.position ).toVar();

				If( surface.valid.and( level.greaterThanEqual( 0 ) ), () => {

					const slot = this._lookup( 0, this._keyOf( this._surfaceCell( surface.position, surface.normal, level, 0 ), level ), this._groupOf( surface.normal ) ).toVar();

					If( slot.notEqual( NONE ), () => {

						atomicAdd( this._rayCount.element( slot ), uint( 1 ) );

					} );

				} );

			} );

		} )().computeKernel().setName( 'RC.Count' );

		const trace = Fn( () => {

			const { pixel, valid } = tracedPixel( instanceIndex );

			If( valid, () => {

				const surface = this._pixelSurface( pixel );
				const index = uint( pixel.y ).mul( width ).add( uint( pixel.x ) ).toVar();

				// pixels without a ray this frame have no hit

				this._hits.element( index.mul( 2 ) ).w.assign( 0 );

				If( surface.valid, () => {

					this._tracePrimary( surface, index );

				} );

			} );

		} )().computeKernel().setName( 'RC.Trace' );

		const traceSecondary = this.secondary ? Fn( () => {

			If( instanceIndex.lessThan( pixels ), () => {

				const hit = secondaryHit( instanceIndex );

				If( hit.valid, () => {

					this._traceSecondary( hit, instanceIndex );

				} );

			} );

		} )().computeKernel().setName( 'RC.TraceSecondary' ) : null;

		return { gather, gatherSecondary, count, trace, traceSecondary };

	}

	/**
	 * Traces the ray of a pixel in the direction of its probe's R2 sequence, records its hit and deposits it into
	 * the cascades of the levels of detail of the pixel (`compute_rays()` in chandra).
	 *
	 * @private
	 */
	_tracePrimary( surface, index ) {

		const { position, normal } = surface;
		const level = this._primaryLevel( position ).toVar();
		const direction = vec3( 0, 0, 1 ).toVar();

		If( level.greaterThanEqual( 0 ), () => {

			const slot = this._lookup( 0, this._keyOf( this._surfaceCell( position, normal, level, 0 ), level ), this._groupOf( normal ) ).toVar();

			If( slot.notEqual( NONE ), () => {

				const sequence = this._rayOffset.element( slot ).add( atomicAdd( this._rayIndex.element( slot ), uint( 1 ) ) ).toVar();
				const r2 = vec2( float( sequence.mul( R2[ 0 ] ) ), float( sequence.mul( R2[ 1 ] ) ) ).mul( 2 ** - 32 );

				direction.assign( projectLine( fract( r2.add( this._jitter ) ) ) );

			} );

		} );

		direction.assign( select( dot( direction, normal ).lessThan( 0 ), direction.negate(), direction ) );

		const spacing = this._spacingOf( level.max( 0 ), 0 );
		const origin = position.add( normal.mul( spacing.mul( 0.05 ) ) ).toVar();
		const { radiance, distance, point, hitNormal } = this._traceRay( origin, direction );

		If( distance.greaterThanEqual( 0 ), () => {

			this._hits.element( index.mul( 2 ) ).assign( vec4( point, float( this._frameNode ) ) );
			this._hits.element( index.mul( 2 ).add( 1 ) ).assign( vec4( hitNormal, 0 ) );

		} );

		for ( const { level, weight } of this._levelsOf( position ) ) {

			If( weight.greaterThan( 0 ), () => {

				this._depositRay( position, normal, level, direction, radiance, distance, this._maxLifetime );

			} );

		}

	}

	/**
	 * Traces a secondary ray from a hit of the last frame in a random direction and deposits it into the
	 * cascades of its secondary probe (`compute_secondary_rays()` in chandra).
	 *
	 * @private
	 */
	_traceSecondary( hit, index ) {

		const random = pcg3df( uvec3( index, this._frameNode, uint( 173 ) ) );
		const direction = projectLine( random.xy ).toVar();

		direction.assign( select( dot( direction, hit.normal ).lessThan( 0 ), direction.negate(), direction ) );

		const origin = hit.position.add( hit.normal.mul( this._spacingOf( hit.level.sub( 2 ), 0 ).mul( 0.05 ) ) ).toVar();
		const { radiance, distance } = this._traceRay( origin, direction );

		this._depositRay( hit.position, hit.normal, hit.level, direction, radiance, distance, this._secondaryLifetime );

	}

	/**
	 * Traces a ray: the radiance of its hit, or of the sky, the hit distance, or -1, and the position and normal
	 * of the hit.
	 *
	 * @private
	 */
	_traceRay( origin, direction ) {

		const radiosity = this._radiosity;
		const hit = radiosity._trace( origin, direction, float( 0 ) ).toVar();
		const radiance = vec3( 0 ).toVar();
		const distance = float( - 1 ).toVar();
		const point = vec3( 0 ).toVar();
		const hitNormal = vec3( 0 ).toVar();

		If( hit.z.notEqual( 0 ), () => {

			distance.assign( radiosity._hitDistance( hit, origin ) );
			point.assign( origin.add( direction.mul( distance ) ) );
			hitNormal.assign( radiosity._faceNormal( hit, direction ) );
			radiance.assign( this._hitRadiance( hit, point, hitNormal, distance ) );

		} ).Else( () => {

			if ( this._skyTarget !== null ) {

				radiance.assign( texture( this._skyTarget.texture ).sample( equirectUV( direction ) ).level( 0 ).rgb );

			} else {

				radiance.assign( this.skyColor );

			}

		} );

		radiance.assign( radiance.min( this._maxRadiance ) );

		return { radiance, distance, point, hitNormal };

	}

	/**
	 * The radiance that leaves a hit towards the ray: its emission, and its albedo times the light of the
	 * sun and of its secondary probe (`get_emission()` in chandra).
	 *
	 * @private
	 */
	_hitRadiance( hit, point, normal, distance ) {

		const radiosity = this._radiosity;

		// the level of detail of the maps by the footprint of a ray cone, with the spread of a ray of about 32

		const { facing, albedo, emission } = radiosity._hitMaterial( hit, hit.w.add( Math.log2( Math.sqrt( Math.PI / 32 ) ) ) );

		const radiance = vec3( 0 ).toVar();
		const cosine = dot( normal, this._sunDirection ).toVar();

		If( cosine.greaterThan( 0 ).and( this._sunColor.dot( vec3( 1 ) ).greaterThan( 0 ) ), () => {

			radiance.addAssign( this._sunColor.mul( cosine.mul( radiosity._visibility( point.add( normal.mul( distance.mul( 1e-3 ).add( 1e-3 ) ) ), this._sunDirection, float( 0 ) ) ) ).div( Math.PI ) );

		} );

		if ( this.secondary ) radiance.addAssign( this._sampleSecondary( point, normal ) );

		return emission.add( albedo.mul( radiance ) ).mul( facing );

	}

	/**
	 * Deposits a ray into the probes of a surface in a level of detail: its radiance into the cascade whose
	 * interval holds the hit, the last one for rays that leave the scene, and its transmittance into the cascades
	 * below (`compute_deposit_ray()` and `deposit_split()` in chandra). The probe of the finest cascade lives for
	 * `lifetime` frames on.
	 *
	 * @private
	 */
	_depositRay( position, normal, level, direction, radiance, distance, lifetime ) {

		const group = this._groupOf( normal ).toVar();
		const keyed = position.add( normal.mul( this._spacingOf( level, 0 ).mul( KEY_OFFSET ) ) ).toVar();
		const length = this._spacingOf( level, 0 ).mul( this.intervalScale );
		const index = log2( distance.div( length ).max( 1e-6 ) ).div( Math.log2( this.lengthScale ) );
		const cascade = select( distance.lessThan( 0 ), int( this.cascades - 1 ), int( ceil( index.max( 0 ) ) ).min( this.cascades - 1 ) ).toVar();

		for ( let n = 0; n < this.cascades; n ++ ) {

			If( cascade.greaterThanEqual( n ), () => {

				const slot = this._lookup( n, this._keyOf( this._cellOf( keyed, level, n ), level ), group ).toVar();

				if ( n === 0 ) {

					If( slot.notEqual( NONE ), () => {

						atomicMax( this._expiry.element( slot ), this._frameNode.add( lifetime ).sub( 1 ) );

					} );

				}

				If( cascade.equal( n ), () => {

					this._deposit( slot, n, direction, radiance, false );

				} ).Else( () => {

					this._deposit( slot, n, direction, null, true );

				} );

			} );

		}

	}

	/**
	 * Adds a ray to a bin of a probe: its radiance, or that it passed the interval, and counts it for the probe.
	 *
	 * @private
	 */
	_deposit( slot, n, direction, radiance, passed ) {

		const cascade = this._layout.cascades[ n ];

		If( slot.notEqual( NONE ), () => {

			const base = slot.mul( cascade.bins ).add( this._binOf( direction, n ) ).add( cascade.binOffset ).mul( WORDS ).toVar();

			atomicAdd( this._deposited.element( slot.add( cascade.slotOffset ) ), uint( 1 ) );

			const rays = atomicAdd( this._deposits.element( base.add( 4 ) ), uint( 1 ) );

			If( rays.lessThan( MAX_RAYS ), () => {

				if ( radiance !== null ) {

					const value = radiance.max( 0 ).mul( SCALE ).add( 0.5 ).toVar();

					for ( let channel = 0; channel < 3; channel ++ ) {

						const component = uint( value[ 'xyz'[ channel ] ] ).toVar();

						If( component.greaterThan( 0 ), () => {

							atomicAdd( this._deposits.element( base.add( channel ) ), component );

						} );

					}

				}

				if ( passed ) atomicAdd( this._deposits.element( base.add( 3 ) ), uint( 1 ) );

			} );

		} );

	}

}

/**
 * The finalizer of MurmurHash3, which spreads the keys of neighboring cells over a table.
 *
 * @private
 */
function mixKey( key ) {

	const h = uint( key ).toVar();
	h.assign( h.bitXor( h.shiftRight( uint( 16 ) ) ).mul( 0x85ebca6b ) );
	h.assign( h.bitXor( h.shiftRight( uint( 13 ) ) ).mul( 0xc2b2ae35 ) );

	return h.bitXor( h.shiftRight( uint( 16 ) ) );

}

/**
 * The PCG3D hash of three integers on the CPU, see `pcg3df()`.
 *
 * @private
 */
function pcg3d( x, y, z ) {

	x = ( Math.imul( x, 1664525 ) + 1013904223 ) >>> 0;
	y = ( Math.imul( y, 1664525 ) + 1013904223 ) >>> 0;
	z = ( Math.imul( z, 1664525 ) + 1013904223 ) >>> 0;

	x = ( x + Math.imul( y, z ) ) >>> 0;
	y = ( y + Math.imul( z, x ) ) >>> 0;
	z = ( z + Math.imul( x, y ) ) >>> 0;

	x = ( x ^ ( x >>> 16 ) ) >>> 0;
	y = ( y ^ ( y >>> 16 ) ) >>> 0;
	z = ( z ^ ( z >>> 16 ) ) >>> 0;

	x = ( x + Math.imul( y, z ) ) >>> 0;
	y = ( y + Math.imul( z, x ) ) >>> 0;
	z = ( z + Math.imul( x, y ) ) >>> 0;

	return [ x, y, z ];

}

/**
 * The PCG3D hash of three integers as floats in [0, 1].
 *
 * @private
 */
function pcg3df( value ) {

	const v = value.mul( 1664525 ).add( 1013904223 ).toVar();

	v.x.addAssign( v.y.mul( v.z ) );
	v.y.addAssign( v.z.mul( v.x ) );
	v.z.addAssign( v.x.mul( v.y ) );

	v.bitXorAssign( v.shiftRight( uvec3( 16 ) ) );

	v.x.addAssign( v.y.mul( v.z ) );
	v.y.addAssign( v.z.mul( v.x ) );
	v.z.addAssign( v.x.mul( v.y ) );

	return vec3( v ).div( 0xffffffff );

}

/**
 * A uniformly distributed line through the origin of a point in [0, 1]^2, as a direction of the upper half
 * of the sphere along y (`project_line()` in chandra).
 *
 * @private
 */
function projectLine( point ) {

	const phi = point.x.mul( Math.PI ).toVar();
	const z = point.y.mul( 2 ).sub( 1 ).toVar();
	const r = sqrt( z.mul( z ).oneMinus().max( 0 ) );

	return vec3( r.mul( cos( phi ) ), r.mul( sin( phi ) ), z );

}

/**
 * The center direction of a bin of a cascade with `theta` bins along the polar axis.
 *
 * @private
 */
function binDirection( bin, theta ) {

	const u = float( bin.mod( 2 * theta ) ).add( 0.5 ).div( 2 * theta );
	const z = float( bin.div( 2 * theta ) ).add( 0.5 ).div( theta ).mul( 2 ).sub( 1 ).toVar();
	const r = sqrt( z.mul( z ).oneMinus().max( 0 ) );
	const phi = u.mul( 2 * Math.PI ).toVar();

	return vec3( r.mul( cos( phi ) ), r.mul( sin( phi ) ), z );

}

/**
 * The octahedral coordinates of a direction in [0, 1]^2.
 *
 * @private
 */
function octahedralEncode( direction ) {

	const n = direction.div( abs( direction.x ).add( abs( direction.y ) ).add( abs( direction.z ) ) ).toVar();
	const signs = select( n.xy.greaterThanEqual( vec2( 0 ) ), vec2( 1 ), vec2( - 1 ) );
	const folded = select( n.z.greaterThanEqual( 0 ), n.xy, abs( n.yx ).oneMinus().mul( signs ) );

	return folded.mul( 0.5 ).add( 0.5 );

}

/**
 * The direction of octahedral coordinates in [0, 1]^2.
 *
 * @private
 */
function octahedralDecode( coordinates ) {

	const v = coordinates.mul( 2 ).sub( 1 ).toVar();
	const f = abs( v.x ).add( abs( v.y ) ).oneMinus().toVar();
	const signs = select( v.greaterThanEqual( vec2( 0 ) ), vec2( 1 ), vec2( - 1 ) );
	const n = select( f.greaterThanEqual( 0 ), v, abs( v.yx ).oneMinus().mul( signs ) );

	return normalize( vec3( n, f ) );

}

/**
 * The index of a texel of an octahedral map, with the texels beyond its edges mirrored across them as the
 * octahedron folds.
 *
 * @private
 */
function octahedralTexel( texel ) {

	const last = IRRADIANCE_SIZE - 1;
	const x = texel.x.toVar(), y = texel.y.toVar();
	const outsideX = x.lessThan( 0 ).or( x.greaterThan( last ) ).toVar();
	const outsideY = y.lessThan( 0 ).or( y.greaterThan( last ) ).toVar();

	If( outsideX.and( outsideY ), () => {

		// the opposite corner

		x.assign( select( x.lessThan( 0 ), int( last ), int( 0 ) ) );
		y.assign( select( y.lessThan( 0 ), int( last ), int( 0 ) ) );

	} ).ElseIf( outsideY, () => {

		x.assign( int( last ).sub( x ) );
		y.assign( y.clamp( 0, last ) );

	} ).ElseIf( outsideX, () => {

		y.assign( int( last ).sub( y ) );
		x.assign( x.clamp( 0, last ) );

	} );

	return uint( y.mul( IRRADIANCE_SIZE ).add( x ) );

}

export { RealtimeRadianceCascades };
