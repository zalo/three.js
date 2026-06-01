import { RenderTarget, Vector2, Vector3, Matrix4, TempNode, QuadMesh, NodeMaterial, RendererUtils, Storage3DTexture, HalfFloatType } from 'three/webgpu';
import { Fn, If, Loop, Break, uniform, uv, vec2, vec3, vec4, float, int, uint, ivec2, ivec3, instanceIndex, textureStore, texture, texture3D, getViewPosition, normalize, cross, min, max, clamp, fract, sin, cos, sqrt, abs, rand, PI, passTexture, convertToTexture, NodeUpdateType } from 'three/tsl';

const _quadMesh = /*@__PURE__*/ new QuadMesh();
const _size = /*@__PURE__*/ new Vector2();

let _rendererState;

/**
 * Post processing node implementing a voxel based, world-space global illumination effect.
 *
 * This is a TSL port of the core idea behind Godot's HDDAGI (Hierarchical Dynamic Distance
 * Field Global Illumination, a full upgrade to SDFGI). The scene's direct lighting is captured
 * into a 3D radiance volume, which is then ray-marched at shading time to gather indirect
 * diffuse light and ambient occlusion. Unlike screen-space techniques (see {@link SSGINode}),
 * the radiance volume is a world-space representation, so light bounces and occlusion are not
 * limited to what is currently visible on screen.
 *
 * This first iteration implements a single cascade. It is the foundation for the remaining
 * HDDAGI features (cascaded clipmaps, jump-flood signed distance fields, octahedral irradiance
 * probes with temporal filtering) which are tracked as follow-up work.
 *
 * Pipeline (per frame, in {@link HDDAGINode#updateBefore}):
 * 1. Blit: the lit beauty and the world-space surface positions (reconstructed from depth) are
 *    copied into node-owned render targets. Compute shaders cannot reliably sample the depth buffer
 *    or an upstream pass's MRT attachments, so this fragment pass produces compute-readable copies.
 * 2. Voxelization: a clear compute pass zeroes the volume, then a scatter compute pass writes each
 *    visible surface's lit color into the voxel it falls within.
 * 3. Gather: a screen-space pass ray-marches the radiance volume across a cosine-weighted
 *    hemisphere around each surface to accumulate indirect diffuse light + ambient occlusion.
 *
 * References:
 * - {@link https://github.com/godotengine/godot/pull/119869} (Godot HDDAGI).
 *
 * @augments TempNode
 * @three_import import { hddagi } from 'three/addons/tsl/display/HDDAGINode.js';
 */
class HDDAGINode extends TempNode {

	static get type() {

		return 'HDDAGINode';

	}

	/**
	 * Constructs a new HDDAGI node.
	 *
	 * @param {TextureNode} beautyNode - A texture node that represents the direct-lit scene (beauty) pass.
	 * @param {TextureNode} depthNode - A texture node that represents the scene's depth. World-space surface positions are reconstructed from it.
	 * @param {Node} normalNode - A node that yields the scene's view-space normals when sampled.
	 * @param {PerspectiveCamera} camera - The camera the scene is rendered with.
	 * @param {number} [gridSize=32] - The resolution of the cubic radiance volume along each axis.
	 */
	constructor( beautyNode, depthNode, normalNode, camera, gridSize = 32 ) {

		super( 'vec4' );

		/**
		 * A texture node that represents the direct-lit scene (beauty) pass.
		 *
		 * @type {TextureNode}
		 */
		this.beautyNode = beautyNode;

		/**
		 * A texture node that represents the scene's depth.
		 *
		 * @type {TextureNode}
		 */
		this.depthNode = depthNode;

		/**
		 * A node that yields the scene's view-space normals when sampled.
		 *
		 * @type {Node}
		 */
		this.normalNode = normalNode;

		/**
		 * The resolution of the cubic radiance volume along each axis.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.gridSize = gridSize;

		/**
		 * The `updateBeforeType` is set to `NodeUpdateType.FRAME` since the node renders
		 * its effect once per frame in `updateBefore()`.
		 *
		 * @type {string}
		 * @default 'frame'
		 */
		this.updateBeforeType = NodeUpdateType.FRAME;

		/**
		 * Number of hemisphere rays marched per pixel. Higher values reduce noise at a linear cost.
		 *
		 * @type {UniformNode<uint>}
		 * @default 8
		 */
		this.rayCount = uniform( 8, 'uint' );

		/**
		 * Number of steps taken when marching the radiance volume along a single ray.
		 *
		 * @type {UniformNode<uint>}
		 * @default 32
		 */
		this.stepCount = uniform( 32, 'uint' );

		/**
		 * Intensity of the gathered indirect diffuse light.
		 *
		 * @type {UniformNode<float>}
		 * @default 4
		 */
		this.giIntensity = uniform( 8, 'float' );

		/**
		 * Intensity of the ambient occlusion derived from ray hits.
		 *
		 * @type {UniformNode<float>}
		 * @default 1
		 */
		this.aoIntensity = uniform( 1, 'float' );

		/**
		 * Maximum world-space distance a ray travels through the volume.
		 *
		 * @type {UniformNode<float>}
		 * @default 30
		 */
		this.maxDistance = uniform( 30, 'float' );

		/**
		 * World-space offset (in voxels) applied to the ray origin to avoid self-intersection.
		 *
		 * @type {UniformNode<float>}
		 * @default 2
		 */
		this.normalBias = uniform( 2, 'float' );

		/**
		 * Step size along a ray, expressed as a multiple of a voxel's world size.
		 *
		 * @type {UniformNode<float>}
		 * @default 1
		 */
		this.rayStep = uniform( 1, 'float' );

		/**
		 * Opacity threshold above which a marched voxel is considered a hit.
		 *
		 * @type {UniformNode<float>}
		 * @default 0.05
		 */
		this.opacityThreshold = uniform( 0.05, 'float' );

		/**
		 * Debug visualization mode (written to the effect's `rgb` output):
		 * - `0`: the gathered GI (default).
		 * - `1`: the radiance volume sampled at each surface point.
		 * - `2`: voxel occupancy at each surface point (white where a voxel is filled).
		 * - `3`: the radiance volume ray-marched from the camera, i.e. the voxelized scene structure.
		 * - `4`: the reconstructed world position, normalized into the volume (sanity-checks the depth
		 *   reconstruction - should be a smooth gradient across the scene).
		 * - `5`: the raw sampled depth (mid-gray for geometry, white at the far plane).
		 * - `6`: the raw reconstructed world position, scaled so the world origin is mid-gray.
		 *
		 * @type {UniformNode<uint>}
		 * @default 0
		 */
		this.debug = uniform( 0, 'uint' );

		/**
		 * Whether to use temporal filtering. When `true`, the per-frame noise is jittered so the
		 * result converges when combined with a temporal anti-aliasing pass (see {@link TRAANode}).
		 *
		 * @type {boolean}
		 * @default true
		 */
		this.useTemporalFiltering = true;

		// private uniforms

		/**
		 * The resolution of the effect.
		 *
		 * @private
		 * @type {UniformNode<vec2>}
		 */
		this._resolution = uniform( new Vector2() );

		/**
		 * Minimum (corner) of the volume's world-space bounding box.
		 *
		 * @private
		 * @type {UniformNode<vec3>}
		 */
		this._volumeMin = uniform( new Vector3( - 12, - 4.5, - 12 ) );

		/**
		 * World-space side length of the cubic volume.
		 *
		 * @private
		 * @type {UniformNode<float>}
		 */
		this._volumeSize = uniform( 24 );

		/**
		 * Center of the volume in world space (used by {@link HDDAGINode#setVolume}).
		 *
		 * @private
		 * @type {Vector3}
		 */
		this._volumeCenter = new Vector3( 0, 7.5, 0 );

		/**
		 * The camera's world matrix, used to transform view-space positions/normals to world space.
		 * Copied explicitly from the camera each frame in {@link HDDAGINode#updateBefore} so the
		 * reconstruction does not depend on uniform auto-tracking timing.
		 *
		 * @private
		 * @type {UniformNode<mat4>}
		 */
		this._cameraMatrixWorld = uniform( new Matrix4() );

		/**
		 * The camera's inverse projection matrix, used to reconstruct view-space positions from depth.
		 *
		 * @private
		 * @type {UniformNode<mat4>}
		 */
		this._projectionMatrixInverse = uniform( new Matrix4() );

		/**
		 * Temporal jitter applied to the per-pixel noise.
		 *
		 * @private
		 * @type {UniformNode<float>}
		 */
		this._temporalJitter = uniform( 0 );

		/**
		 * A reference to the scene's camera.
		 *
		 * @private
		 * @type {PerspectiveCamera}
		 */
		this._camera = camera;

		/**
		 * The world-space radiance volume populated by the voxelization pass.
		 *
		 * @private
		 * @type {Storage3DTexture}
		 */
		this._radianceVolume = new Storage3DTexture( gridSize, gridSize, gridSize );
		this._radianceVolume.type = HalfFloatType;
		this._radianceVolume.generateMipmaps = false;
		this._radianceVolume.name = 'HDDAGI.radiance';

		/**
		 * Compute nodes that clear and populate the radiance volume. The scatter node is built lazily
		 * (and rebuilt on resize) because its dispatch size depends on the resolution.
		 *
		 * @private
		 * @type {?ComputeNode}
		 */
		this._clearNode = null;

		/**
		 * @private
		 * @type {?ComputeNode}
		 */
		this._scatterNode = null;

		/**
		 * Node-owned copies of the beauty and world-position inputs. The voxelization compute pass
		 * reads from these textures rather than the input pass textures directly: a single-attachment
		 * render target that the node renders itself is reliably bindable from a compute shader,
		 * whereas an upstream pass's MRT attachment is not.
		 *
		 * @private
		 * @type {RenderTarget}
		 */
		this._beautyRT = new RenderTarget( 1, 1, { depthBuffer: false } );
		this._beautyRT.texture.type = HalfFloatType;
		this._beautyRT.texture.name = 'HDDAGI.beauty';

		/**
		 * @private
		 * @type {RenderTarget}
		 */
		this._positionRT = new RenderTarget( 1, 1, { depthBuffer: false } );
		this._positionRT.texture.type = HalfFloatType;
		this._positionRT.texture.name = 'HDDAGI.position';

		/**
		 * Materials that copy the inputs into the node-owned render targets.
		 *
		 * @private
		 * @type {NodeMaterial}
		 */
		this._blitBeautyMaterial = new NodeMaterial();
		this._blitBeautyMaterial.name = 'HDDAGI.blitBeauty';

		/**
		 * @private
		 * @type {NodeMaterial}
		 */
		this._blitPositionMaterial = new NodeMaterial();
		this._blitPositionMaterial.name = 'HDDAGI.blitPosition';

		/**
		 * The render target the gathered GI is rendered into.
		 *
		 * @private
		 * @type {RenderTarget}
		 */
		this._renderTarget = new RenderTarget( 1, 1, { depthBuffer: false } );
		this._renderTarget.texture.name = 'HDDAGI';

		/**
		 * The material that is used to render the gather pass.
		 *
		 * @private
		 * @type {NodeMaterial}
		 */
		this._material = new NodeMaterial();
		this._material.name = 'HDDAGI';

		/**
		 * The result of the effect is represented as a separate texture node.
		 *
		 * @private
		 * @type {PassTextureNode}
		 */
		this._textureNode = passTexture( this, this._renderTarget.texture );

	}

	/**
	 * Returns the result of the effect as a texture node.
	 *
	 * @return {PassTextureNode} A texture node that represents the result of the effect.
	 */
	getTextureNode() {

		return this._textureNode;

	}

	/**
	 * Positions and sizes the world-space radiance volume. The volume is a cube; choose a center
	 * and side length that enclose the geometry that should contribute to global illumination.
	 *
	 * @param {Vector3} center - The world-space center of the volume.
	 * @param {number} size - The world-space side length of the cubic volume.
	 * @return {HDDAGINode} A reference to this node.
	 */
	setVolume( center, size ) {

		this._volumeCenter.copy( center );
		this._volumeSize.value = size;
		this._volumeMin.value.set( center.x - size / 2, center.y - size / 2, center.z - size / 2 );

		return this;

	}

	/**
	 * Sets the size of the effect.
	 *
	 * @param {number} width - The width of the effect.
	 * @param {number} height - The height of the effect.
	 */
	setSize( width, height ) {

		this._resolution.value.set( width, height );
		this._renderTarget.setSize( width, height );
		this._beautyRT.setSize( width, height );
		this._positionRT.setSize( width, height );

	}

	/**
	 * This method is used to render the effect once per frame.
	 *
	 * @param {NodeFrame} frame - The current node frame.
	 */
	updateBefore( frame ) {

		const { renderer } = frame;

		_rendererState = RendererUtils.resetRendererState( renderer, _rendererState );

		//

		const size = renderer.getDrawingBufferSize( _size );
		const resized = size.width !== this._resolution.value.x || size.height !== this._resolution.value.y;
		this.setSize( size.width, size.height );

		// (Re)build the scatter compute node when the render targets are (re)sized. The dispatch size
		// depends on the resolution and the compute pipeline caches its texture bindings, so it must
		// be recreated with fresh texture nodes that point at the new GPU resources.
		if ( resized || this._scatterNode === null ) {

			this._scatterNode = this._buildScatter( size.width, size.height );

		}

		// update camera derived matrices (copied explicitly so world-space reconstruction is correct
		// regardless of uniform auto-tracking timing)

		const camera = this._camera;
		camera.updateMatrixWorld();
		this._cameraMatrixWorld.value.copy( camera.matrixWorld );
		this._projectionMatrixInverse.value.copy( camera.projectionMatrixInverse );

		// temporal jitter

		this._temporalJitter.value = this.useTemporalFiltering === true ? ( frame.frameId % 16 ) * 0.0625 : 0;

		// copy the inputs into the node-owned render targets (this also forces the input passes to render)

		_quadMesh.material = this._blitBeautyMaterial;
		_quadMesh.name = 'HDDAGI.blitBeauty';
		renderer.setRenderTarget( this._beautyRT );
		_quadMesh.render( renderer );

		_quadMesh.material = this._blitPositionMaterial;
		_quadMesh.name = 'HDDAGI.blitPosition';
		renderer.setRenderTarget( this._positionRT );
		_quadMesh.render( renderer );

		// voxelize the scene into the radiance volume (clear, then scatter the lit G-buffer into voxels)

		renderer.compute( this._clearNode );
		renderer.compute( this._scatterNode );

		// gather

		_quadMesh.material = this._material;
		_quadMesh.name = 'HDDAGI';

		renderer.setRenderTarget( this._renderTarget );
		_quadMesh.render( renderer );

		// restore

		RendererUtils.restoreRendererState( renderer, _rendererState );

	}

	/**
	 * This method is used to setup the effect's TSL code.
	 *
	 * @param {NodeBuilder} builder - The current node builder.
	 * @return {PassTextureNode}
	 */
	setup( builder ) {

		const GRID = this.gridSize;
		const gridF = float( GRID );

		// world size of a single voxel

		const voxelWorldSize = this._volumeSize.div( gridF ).toVar( 'voxelWorldSize' );

		// --- blit passes: copy inputs into node-owned, compute-readable render targets -------------

		this._blitBeautyMaterial.fragmentNode = vec4( this.beautyNode.rgb, 1.0 );
		this._blitBeautyMaterial.needsUpdate = true;

		// reconstruct world-space surface positions from depth (avoids a dedicated position MRT
		// attachment, which would exceed the device's color-attachment byte budget). `w` flags a
		// valid surface (background depth of 1 is excluded).
		this._blitPositionMaterial.fragmentNode = Fn( () => {

			const uvNode = uv();
			const depth = this.depthNode.sample( uvNode ).r;
			const viewPos = getViewPosition( uvNode, depth, this._projectionMatrixInverse );
			const worldPos = this._cameraMatrixWorld.mul( vec4( viewPos, 1.0 ) ).xyz;

			return vec4( worldPos, depth.lessThan( 1.0 ).select( 1.0, 0.0 ) );

		} )();
		this._blitPositionMaterial.needsUpdate = true;

		// --- voxelization compute pass ------------------------------------------------------------
		//
		// Rebuilt whenever the render targets are (re)sized. `setSize()` disposes the underlying GPU
		// textures, so the compute pipeline (which caches its bindings) must be recreated with fresh
		// texture nodes that point at the new GPU resources.

		// Clear pass: zero every voxel (the scatter pass only writes occupied voxels).
		this._buildClear = () => Fn( () => {

			const id = instanceIndex;
			const gx = id.mod( uint( GRID ) );
			const gy = id.div( uint( GRID ) ).mod( uint( GRID ) );
			const gz = id.div( uint( GRID * GRID ) );

			textureStore( this._radianceVolume, ivec3( int( gx ), int( gy ), int( gz ) ), vec4( 0 ) );

		} )().compute( GRID * GRID * GRID );

		// Scatter pass: one thread per G-buffer pixel writes that surface's lit color into the voxel
		// it falls within. `width` is baked into the dispatch so the node is rebuilt on resize.
		this._buildScatter = ( width, height ) => Fn( () => {

			const beautyTexture = texture( this._beautyRT.texture );
			const positionTexture = texture( this._positionRT.texture );

			const id = instanceIndex;
			const px = ivec2( int( id.mod( uint( width ) ) ), int( id.div( uint( width ) ) ) ).toVar();

			const surface = positionTexture.load( px ).toVar();

			If( surface.w.greaterThan( 0.5 ), () => {

				const cell = surface.xyz.sub( this._volumeMin ).div( this._volumeSize ).mul( gridF );
				const vi = ivec3( cell ).toVar();

				If( vi.x.greaterThanEqual( int( 0 ) ).and( vi.x.lessThan( int( GRID ) ) ).and( vi.y.greaterThanEqual( int( 0 ) ) ).and( vi.y.lessThan( int( GRID ) ) ).and( vi.z.greaterThanEqual( int( 0 ) ) ).and( vi.z.lessThan( int( GRID ) ) ), () => {

					textureStore( this._radianceVolume, vi, vec4( beautyTexture.load( px ).rgb, 1.0 ) );

				} );

			} );

		} )().compute( width * height );

		this._clearNode = this._buildClear();

		// the scatter dispatch depends on the resolution, so it is built lazily in updateBefore()
		this._scatterNode = null;

		// --- gather pass --------------------------------------------------------------------------

		const radianceTexture = texture3D( this._radianceVolume );

		const gather = Fn( () => {

			const uvNode = uv();

			const depth = this.depthNode.sample( uvNode ).r.toVar();
			depth.greaterThanEqual( 1.0 ).discard(); // background

			const viewPos = getViewPosition( uvNode, depth, this._projectionMatrixInverse );
			const worldPos = this._cameraMatrixWorld.mul( vec4( viewPos, 1.0 ) ).xyz.toVar();

			const viewNormal = this.normalNode.sample( uvNode ).xyz;
			const worldNormal = normalize( this._cameraMatrixWorld.mul( vec4( viewNormal, 0.0 ) ).xyz ).toVar();

			// orthonormal basis around the world normal

			const up = abs( worldNormal.y ).lessThan( 0.999 ).select( vec3( 0, 1, 0 ), vec3( 1, 0, 0 ) );
			const tangent = normalize( cross( up, worldNormal ) ).toVar();
			const bitangent = cross( worldNormal, tangent ).toVar();

			const origin = worldPos.add( worldNormal.mul( voxelWorldSize.mul( this.normalBias ) ) ).toVar();
			const stepSize = voxelWorldSize.mul( this.rayStep ).toVar();

			const giAccum = vec3( 0 ).toVar();
			const occlusion = float( 0 ).toVar();

			const RAY_COUNT = this.rayCount.toConst();
			const STEP_COUNT = this.stepCount.toConst();

			Loop( { start: uint( 0 ), end: RAY_COUNT, type: 'uint', condition: '<' }, ( { i } ) => {

				// cosine-weighted hemisphere sample, jittered per pixel and frame

				const seed = uvNode.add( vec2( float( i ).mul( 0.7548776 ), float( i ).mul( 0.5698403 ) ) ).add( this._temporalJitter ).toVar();
				const u1 = fract( rand( seed ) ).toVar();
				const u2 = fract( rand( seed.add( 0.5 ) ) ).toVar();

				const r = sqrt( u1 );
				const phi = u2.mul( PI ).mul( 2.0 );
				const dir = tangent.mul( r.mul( cos( phi ) ) ).add( bitangent.mul( r.mul( sin( phi ) ) ) ).add( worldNormal.mul( sqrt( max( 0.0, u1.oneMinus() ) ) ) ).toVar();

				// march the radiance volume

				const t = stepSize.toVar();
				const hit = float( 0 ).toVar();
				const hitDistance = this.maxDistance.toVar();
				const hitRadiance = vec3( 0 ).toVar();

				Loop( { start: uint( 0 ), end: STEP_COUNT, type: 'uint', condition: '<' }, () => {

					const p = origin.add( dir.mul( t ) );
					const uvw = p.sub( this._volumeMin ).div( this._volumeSize ).toVar();

					If( uvw.x.lessThan( 0.0 ).or( uvw.y.lessThan( 0.0 ) ).or( uvw.z.lessThan( 0.0 ) ).or( uvw.x.greaterThan( 1.0 ) ).or( uvw.y.greaterThan( 1.0 ) ).or( uvw.z.greaterThan( 1.0 ) ), () => {

						Break();

					} );

					const sample = radianceTexture.sample( uvw );

					If( sample.a.greaterThan( this.opacityThreshold ), () => {

						hitRadiance.assign( sample.rgb );
						hit.assign( 1.0 );
						hitDistance.assign( t );
						Break();

					} );

					t.addAssign( stepSize );

					If( t.greaterThan( this.maxDistance ), () => {

						Break();

					} );

				} );

				giAccum.addAssign( hitRadiance );
				occlusion.addAssign( hit.mul( clamp( this.maxDistance.sub( hitDistance ).div( this.maxDistance ) ) ) );

			} );

			const invRayCount = float( 1.0 ).div( float( RAY_COUNT ) );
			const gi = giAccum.mul( invRayCount ).mul( this.giIntensity );
			const ao = occlusion.mul( invRayCount ).mul( this.aoIntensity ).oneMinus().clamp();

			const result = vec4( gi, ao ).toVar();

			// --- debug visualizations (guarded so they only run when selected) --------------------

			const surfaceUVW = worldPos.sub( this._volumeMin ).div( this._volumeSize );

			If( this.debug.equal( uint( 1 ) ), () => { // radiance volume at the surface

				result.assign( vec4( radianceTexture.sample( surfaceUVW ).rgb, 1.0 ) );

			} );

			If( this.debug.equal( uint( 2 ) ), () => { // voxel occupancy at the surface

				result.assign( vec4( vec3( radianceTexture.sample( surfaceUVW ).a ), 1.0 ) );

			} );

			If( this.debug.equal( uint( 3 ) ), () => { // ray-march the volume from the camera

				const ro = this._cameraMatrixWorld.mul( vec4( 0, 0, 0, 1 ) ).xyz;
				const rd = worldPos.sub( ro ).normalize();
				const invRd = vec3( 1.0 ).div( rd );

				// intersect the volume's bounding box
				const tA = this._volumeMin.sub( ro ).mul( invRd );
				const tB = this._volumeMin.add( this._volumeSize ).sub( ro ).mul( invRd );
				const tMin = min( tA, tB );
				const tMax = max( tA, tB );
				const tStart = max( max( max( tMin.x, tMin.y ), tMin.z ), 0.0 ).toVar();
				const tEnd = min( min( min( tMax.x, tMax.y ), tMax.z ), worldPos.sub( ro ).length() );

				const stepSize = voxelWorldSize.mul( this.rayStep );
				const voxColor = vec3( 0 ).toVar();

				Loop( { start: uint( 0 ), end: uint( 256 ), type: 'uint', condition: '<' }, () => {

					If( tStart.greaterThan( tEnd ), () => {

						Break();

					} );

					const s = radianceTexture.sample( ro.add( rd.mul( tStart ) ).sub( this._volumeMin ).div( this._volumeSize ) );

					If( s.a.greaterThan( this.opacityThreshold ), () => {

						voxColor.assign( s.rgb );
						Break();

					} );

					tStart.addAssign( stepSize );

				} );

				result.assign( vec4( voxColor, 1.0 ) );

			} );

			If( this.debug.equal( uint( 4 ) ), () => { // reconstructed world position, normalized into the volume

				result.assign( vec4( surfaceUVW, 1.0 ) );

			} );

			If( this.debug.equal( uint( 5 ) ), () => { // raw sampled depth (mid-gray for geometry, white at far plane)

				result.assign( vec4( vec3( depth ), 1.0 ) );

			} );

			If( this.debug.equal( uint( 6 ) ), () => { // raw reconstructed world position, scaled (0.5 = origin)

				result.assign( vec4( worldPos.mul( 0.04 ).add( 0.5 ), 1.0 ) );

			} );

			return result;

		} );

		this._material.fragmentNode = gather().context( builder.getSharedContext() );
		this._material.needsUpdate = true;

		return this._textureNode;

	}

	/**
	 * Frees internal resources. This method should be called
	 * when the effect is no longer required.
	 */
	dispose() {

		this._renderTarget.dispose();
		this._beautyRT.dispose();
		this._positionRT.dispose();
		this._radianceVolume.dispose();
		this._material.dispose();
		this._blitBeautyMaterial.dispose();
		this._blitPositionMaterial.dispose();

	}

}

export default HDDAGINode;

/**
 * TSL function for creating a voxel based, world-space global illumination effect (HDDAGI).
 *
 * @tsl
 * @function
 * @param {Node} beautyNode - A node that represents the direct-lit scene (beauty) pass.
 * @param {TextureNode} depthNode - A texture node that represents the scene's depth.
 * @param {Node} normalNode - A node that yields the scene's view-space normals when sampled.
 * @param {PerspectiveCamera} camera - The camera the scene is rendered with.
 * @param {number} [gridSize=32] - The resolution of the cubic radiance volume along each axis.
 * @returns {HDDAGINode}
 */
export const hddagi = ( beautyNode, depthNode, normalNode, camera, gridSize ) => new HDDAGINode( convertToTexture( beautyNode ), depthNode, normalNode, camera, gridSize );
