import {
	DepthTexture,
	FloatType,
	Matrix4,
	Mesh,
	MeshBasicNodeMaterial,
	NearestFilter,
	RedFormat,
	RenderTarget,
	Scene,
	StorageTexture,
	Vector2,
	Vector4
} from 'three/webgpu';

import { Fn, If, instanceIndex, ivec2, max, min, normalLocal, positionLocal, select, textureLoad, textureStore, uint, uniform, uvec2, vec4 } from 'three/tsl';

const _size = /*@__PURE__*/ new Vector2();

// how far a proxy is pushed inward, per object: the materials of the proxies are alike, so they share a shader and the
// uniform

const _offset = /*@__PURE__*/ uniform( 0 ).onObjectUpdate( ( { object } ) => object.userData.offset );

/**
 * A hierarchical depth buffer of occluders for {@link AdaptiveSubdivisionMesh}: leaves of meshes
 * whose `occlusion` is set to it are culled and not refined while they are hidden behind the
 * occluders, like the parts of a terrain behind a hill. Call {@link AdaptiveSubdivisionOcclusion#update}
 * with the camera each frame before updating the meshes.
 *
 * The occluders are drawn into a small depth buffer with their source geometry, pushed inward by their
 * maximum displacement, so they are never in front of the displaced surface, and the depth buffer is
 * reduced to the farthest depth per texel at every mip level by compute shaders. A leaf is hidden if
 * its bounds are behind the farthest depth of the texels they cover, at the level where they cover at
 * most two by two. As the depth is drawn for the current camera, culling is exact for the frame and
 * leaves never disappear while visible.
 *
 * The levels are laid out like a mip chain in two textures, the even levels in the first and the odd
 * ones in the second, so each level is reduced from a texture it does not write: level 0 at the
 * origin and the others in a column to its right.
 *
 * Call {@link AdaptiveSubdivisionOcclusion#compileAsync} before the first update to compile its
 * shaders without blocking.
 *
 * ```js
 * const occlusion = new AdaptiveSubdivisionOcclusion( meshes );
 *
 * for ( const mesh of meshes ) mesh.occlusion = occlusion;
 *
 * // every frame
 * occlusion.update( renderer, camera );
 *
 * for ( const mesh of meshes ) mesh.update( renderer, camera );
 * ```
 *
 * @three_import import { AdaptiveSubdivisionOcclusion } from 'three/addons/objects/AdaptiveSubdivisionOcclusion.js';
 */
class AdaptiveSubdivisionOcclusion {

	/**
	 * Constructs new occlusion.
	 *
	 * @param {Array<Mesh>} [occluders=[]] - The occluding meshes, adaptive subdivision meshes or regular meshes, which are drawn without displacement.
	 * @param {number} [resolution=256] - The width of the depth buffer, its height follows the aspect of the renderer.
	 */
	constructor( occluders = [], resolution = 256 ) {

		/**
		 * The occluding meshes.
		 *
		 * @type {Array<Mesh>}
		 */
		this.occluders = occluders;

		/**
		 * The width of the depth buffer.
		 *
		 * @type {number}
		 * @default 256
		 */
		this.resolution = resolution;

		/**
		 * The hierarchical depth, the farthest depth of the texels of the level below per texel: the even levels and the
		 * odd levels, empty until the first update.
		 *
		 * @type {Array<StorageTexture>}
		 */
		this.textures = [];

		/**
		 * The rectangles of the levels in the textures, as `{ x, y, width, height }` in texels.
		 *
		 * @type {Array<Object>}
		 */
		this.levels = [];

		/**
		 * The camera of the last update.
		 *
		 * @type {?Camera}
		 */
		this.camera = null;

		/**
		 * The view projection matrix of the last update.
		 *
		 * @type {Matrix4}
		 */
		this.viewProjectionMatrix = new Matrix4();

		this._scene = new Scene();
		this._scene.name = 'AdaptiveSubdivisionOcclusion';
		this._proxies = new Map();
		this._depthTarget = null;
		this._levelNodes = [];
		this._levelNodes.name = 'AdaptiveSubdivisionOcclusion Levels';
		this._reversed = uniform( 0 );
		this._compiling = null;

	}

	/**
	 * The number of mip levels of the hierarchical depth.
	 *
	 * @type {number}
	 * @readonly
	 */
	get levelCount() {

		return this.levels.length;

	}

	/**
	 * Compiles the compute pipelines building the hierarchical depth for the current size of the
	 * renderer, without blocking, see {@link AdaptiveSubdivisionMesh#compileAsync}. Updates are
	 * skipped until they are compiled.
	 *
	 * @async
	 * @param {Renderer} renderer - The renderer.
	 * @return {Promise} A promise that resolves when the pipelines are compiled.
	 */
	async compileAsync( renderer ) {

		await renderer.init();

		if ( this._compiling === null ) {

			this._prepare( renderer );

			// the levels above the first share a shader: browsers may compile a shader again for every pipeline
			// requested while it compiles, so one level compiles it, and the others find it compiled

			const [ first, second, ...rest ] = this._levelNodes;

			this._compiling = renderer.compileComputeAsync( [ first, second ].filter( Boolean ) )
				.then( () => rest.length > 0 ? renderer.compileComputeAsync( rest ) : null )
				.finally( () => this._compiling = null );

		}

		await this._compiling;

	}

	/**
	 * Draws the occluders for the given camera and builds the hierarchical depth.
	 *
	 * @param {Renderer} renderer - The renderer.
	 * @param {Camera} camera - The camera.
	 */
	update( renderer, camera ) {

		if ( this._compiling !== null ) {

			// the meshes do not cull until the first update

			this.camera = null;

			return;

		}

		this._prepare( renderer );

		camera.updateMatrixWorld();

		this._reversed.value = camera.reversedDepth === true ? 1 : 0;

		const renderTarget = renderer.getRenderTarget();

		renderer.setRenderTarget( this._depthTarget );
		renderer.render( this._scene, camera );
		renderer.setRenderTarget( renderTarget );

		// the levels, one dispatch each

		renderer.compute( this._levelNodes );

		this.camera = camera;
		this.viewProjectionMatrix.multiplyMatrices( camera.projectionMatrix, camera.matrixWorldInverse );

	}

	/**
	 * Frees the GPU-related resources allocated by this instance.
	 */
	dispose() {

		this._disposeTargets();

		for ( const proxy of this._proxies.values() ) proxy.material.dispose();

		this._proxies.clear();

	}

	/**
	 * Creates the targets for the size of the renderer and updates the proxies.
	 *
	 * @private
	 * @param {Renderer} renderer - The renderer.
	 */
	_prepare( renderer ) {

		renderer.getDrawingBufferSize( _size );

		const width = this.resolution;
		const height = Math.max( Math.round( width * _size.y / _size.x ), 1 );

		if ( this._depthTarget === null || this._depthTarget.width !== width || this._depthTarget.height !== height ) this._createTargets( width, height );

		this._updateProxies();

	}

	/**
	 * Creates or updates the proxies drawing the occluders, with their world matrices.
	 *
	 * @private
	 */
	_updateProxies() {

		for ( const [ occluder, proxy ] of this._proxies ) {

			if ( this.occluders.includes( occluder ) === false ) {

				this._scene.remove( proxy );
				proxy.material.dispose();
				this._proxies.delete( occluder );

			}

		}

		for ( const occluder of this.occluders ) {

			let proxy = this._proxies.get( occluder );

			if ( proxy === undefined ) {

				// the source geometry pushed inward by the maximum displacement, along its normals

				const subdivision = occluder.isAdaptiveSubdivisionMesh === true;
				const geometry = subdivision ? occluder.sourceGeometry : occluder.geometry;

				const material = new MeshBasicNodeMaterial( { colorWrite: false } );
				material.side = occluder.material.side;
				material.positionNode = geometry.hasAttribute( 'normal' ) ? positionLocal.sub( normalLocal.mul( _offset ) ) : positionLocal;

				proxy = new Mesh( geometry, material );
				proxy.userData.offset = subdivision && geometry.hasAttribute( 'normal' ) ? occluder.maxDisplacement : 0;
				proxy.matrixAutoUpdate = false;
				proxy.matrixWorldAutoUpdate = false;
				proxy.frustumCulled = occluder.frustumCulled;

				this._proxies.set( occluder, proxy );
				this._scene.add( proxy );

			}

			occluder.updateWorldMatrix( true, false );

			proxy.matrixWorld.copy( occluder.matrixWorld );
			proxy.visible = occluder.visible;

		}

	}

	/**
	 * Creates the depth buffer, the textures of the levels and the compute nodes reducing them.
	 *
	 * @private
	 * @param {number} width - The width of the depth buffer.
	 * @param {number} height - The height of the depth buffer.
	 */
	_createTargets( width, height ) {

		this._disposeTargets();

		this._depthTarget = new RenderTarget( width, height, { depthTexture: new DepthTexture( width, height, FloatType ) } );

		// the levels halve rounding down, like mip levels, so each texel reduces three by three texels of the level below,
		// which covers the last row and column of levels with an odd size

		const levels = [ { x: 0, y: 0, width, height } ];

		for ( let y = 0; levels[ levels.length - 1 ].width > 1 || levels[ levels.length - 1 ].height > 1; ) {

			const below = levels[ levels.length - 1 ];
			const level = { x: width, y, width: Math.max( Math.floor( below.width / 2 ), 1 ), height: Math.max( Math.floor( below.height / 2 ), 1 ) };

			levels.push( level );
			y += level.height;

		}

		const textureHeight = Math.max( height, levels.reduce( ( sum, level, i ) => sum + ( i > 0 ? level.height : 0 ), 0 ) );

		for ( let i = 0; i < 2; i ++ ) {

			const levelTexture = new StorageTexture( width + levels[ Math.min( 1, levels.length - 1 ) ].width, textureHeight );

			levelTexture.type = FloatType;
			levelTexture.format = RedFormat;
			levelTexture.minFilter = levelTexture.magFilter = NearestFilter;
			levelTexture.generateMipmaps = false;

			this.textures.push( levelTexture );

		}

		this.levels = levels;

		// level 0 copies the depth, the others reduce the level below from the other texture. their rectangles are
		// uniforms, and the name is part of the shader, so all levels above 0 share a shader

		levels.forEach( ( level, index ) => {

			const target = this.textures[ index % 2 ];
			const below = levels[ Math.max( index - 1, 0 ) ];
			const source = index === 0 ? this._depthTarget.depthTexture : this.textures[ ( index - 1 ) % 2 ];

			const rect = uniform( new Vector4( level.x, level.y, level.width, level.height ) );
			const belowRect = uniform( new Vector4( below.x, below.y, below.width, below.height ) );

			const node = Fn( () => {

				const width = uint( rect.z );

				If( instanceIndex.lessThan( width.mul( uint( rect.w ) ) ), () => {

					const texel = ivec2( instanceIndex.mod( width ), instanceIndex.div( width ) ).toVar();

					let depth;

					if ( index === 0 ) {

						depth = textureLoad( source, texel ).x;

					} else {

						const origin = ivec2( belowRect.xy ).toVar();
						const last = ivec2( belowRect.zw ).sub( 1 ).toVar();

						depth = textureLoad( source, origin.add( min( texel.mul( 2 ), last ) ) ).x.toVar();

						for ( let y = 0; y < 3; y ++ ) {

							for ( let x = 0; x < 3; x ++ ) {

								if ( x === 0 && y === 0 ) continue;

								const value = textureLoad( source, origin.add( min( texel.mul( 2 ).add( ivec2( x, y ) ), last ) ) ).x;

								depth.assign( select( this._reversed.equal( 1 ), min( depth, value ), max( depth, value ) ) );

							}

						}

					}

					textureStore( target, uvec2( texel.add( ivec2( rect.xy ) ) ), vec4( depth, 0, 0, 1 ) );

				} );

			} )().compute( level.width * level.height, [ 64 ] ).setName( index === 0 ? 'AdaptiveSubdivisionOcclusion Depth' : 'AdaptiveSubdivisionOcclusion Level' );

			this._levelNodes.push( node );

		} );

	}

	/**
	 * Frees the targets.
	 *
	 * @private
	 */
	_disposeTargets() {

		if ( this._depthTarget !== null ) {

			this._depthTarget.depthTexture.dispose();
			this._depthTarget.dispose();

		}

		for ( const node of this._levelNodes ) node.dispose();
		for ( const levelTexture of this.textures ) levelTexture.dispose();

		this._depthTarget = null;
		this._levelNodes.length = 0;
		this.textures = [];
		this.levels = [];

	}

}

export { AdaptiveSubdivisionOcclusion };
