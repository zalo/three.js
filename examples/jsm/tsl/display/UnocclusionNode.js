import { HalfFloatType, Vector2, RenderTarget, RendererUtils, QuadMesh, NodeMaterial, TempNode, NodeUpdateType, Matrix4, DepthTexture } from 'three/webgpu';
import { float, If, Loop, int, Fn, min, max, clamp, nodeObject, texture, uniform, uv, vec2, vec4, passTexture, getViewPosition, length, mat4 } from 'three/tsl';

const _quadMesh = /*@__PURE__*/ new QuadMesh();

let _rendererState;


/**
 * A special node that computes the Unocclusion Map (regions where objects were recently unoccluded).
 *
 * @augments TempNode
 * @three_import import { Unocclusion } from 'three/addons/tsl/display/UnocclusionNode.js';
 */
class UnocclusionNode extends TempNode {

	static get type() {

		return 'UnocclusionNode';

	}

	/**
	 * Constructs a new Unocclusion node.
	 *
	 * @param {TextureNode} depthNode - A node that represents the scene's depth.
	 * @param {TextureNode} velocityNode - A node that represents the scene's velocity.
	 * @param {Camera} camera - The camera the scene is rendered with.
	 */
	constructor( depthNode, velocityNode, camera ) {

		super( 'float' );

		/**
		 * This flag can be used for type testing.
		 *
		 * @type {boolean}
		 * @readonly
		 * @default true
		 */
		this.isUnocclusionNode = true;

		/**
		 * The `updateBeforeType` is set to `NodeUpdateType.FRAME` since the node renders
		 * its effect once per frame in `updateBefore()`.
		 *
		 * @type {string}
		 * @default 'frame'
		 */
		this.updateBeforeType = NodeUpdateType.FRAME;

		/**
		 * A node that represents the scene's velocity.
		 *
		 * @type {TextureNode}
		 */
		this.depthNode = depthNode;

		/**
		 * A node that represents the scene's velocity.
		 *
		 * @type {TextureNode}
		 */
		this.velocityNode = velocityNode;

		/**
		 *  The camera the scene is rendered with.
		 *
		 * @type {Camera}
		 */
		this.camera = camera;

		/**
		 * A uniform node holding the inverse resolution value.
		 *
		 * @private
		 * @type {UniformNode<vec2>}
		 */
		this._invSize = uniform( new Vector2() );

		/**
		 * A uniform node holding the camera world matrix.
		 *
		 * @private
		 * @type {UniformNode<mat4>}
		 */
		this._cameraWorldMatrix = uniform( new Matrix4() );

		/**
		 * A uniform node holding the camera projection matrix inverse.
		 *
		 * @private
		 * @type {UniformNode<mat4>}
		 */
		this._cameraProjectionMatrixInverse = uniform( new Matrix4() );

		/**
		 * A uniform node holding the previous frame's view matrix.
		 *
		 * @private
		 * @type {UniformNode<mat4>}
		 */
		this._previousCameraWorldMatrix = uniform( new Matrix4() );

		/**
		 * A uniform node holding the previous frame's projection matrix inverse.
		 *
		 * @private
		 * @type {UniformNode<mat4>}
		 */
		this._previousCameraProjectionMatrixInverse = uniform( new Matrix4() );

		/**
		 * The render target for the resolve.
		 *
		 * @private
		 * @type {?RenderTarget}
		 */
		this._resolveRenderTarget = new RenderTarget( 1, 1, { depthBuffer: false, type: HalfFloatType } );
		this._resolveRenderTarget.texture.name = 'UnocclusionNode.resolve';

		/**
		 * Material used for the resolve step.
		 *
		 * @private
		 * @type {NodeMaterial}
		 */
		this._resolveMaterial = new NodeMaterial();
		this._resolveMaterial.name = 'Unocclusion.resolve';

		/**
		 * The result of the effect is represented as a separate texture node.
		 *
		 * @private
		 * @type {PassTextureNode}
		 */
		this._textureNode = passTexture( this, this._resolveRenderTarget.texture );

		/**
		 * Used to save the original/unjittered projection matrix.
		 *
		 * @private
		 * @type {Matrix4}
		 */
		this._originalProjectionMatrix = new Matrix4();

		/**
		 * The previous frame's depth buffer.
		 *
		 * @private
		 * @type {?DepthTexture}
		 */
		this._previousDepthTexture = new DepthTexture( 1, 1 );

		/**
		 * A texture node for the previous depth buffer.
		 *
		 * @private
		 * @type {TextureNode}
		 */
		this._previousDepthNode = texture( new DepthTexture( 1, 1 ) );

		/**
		 * Sync the post processing stack with the Unocclusion node.
		 * @private
		 * @type {boolean}
		 */
		this._needsPostProcessingSync = false;

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
	 * Sets the size of the effect.
	 *
	 * @param {number} width - The width of the effect.
	 * @param {number} height - The height of the effect.
	 */
	setSize( width, height ) {

		this._resolveRenderTarget.setSize( width, height );

		this._invSize.value.set( 1 / width, 1 / height );

	}

	/**
	 * This method is used to render the effect once per frame.
	 *
	 * @param {NodeFrame} frame - The current node frame.
	 */
	updateBefore( frame ) {

		const { renderer } = frame;

		// Store previous frame matrices before updating current ones
		this._previousCameraWorldMatrix.value.copy( this._cameraWorldMatrix.value );
		this._previousCameraProjectionMatrixInverse.value.copy( this._cameraProjectionMatrixInverse.value );

		// Update camera matrices uniforms
		this._cameraWorldMatrix.value.copy( this.camera.matrixWorld );
		this._cameraProjectionMatrixInverse.value.copy( this.camera.projectionMatrixInverse );

		//

		_rendererState = RendererUtils.resetRendererState( renderer, _rendererState );

		// resolve

		renderer.setRenderTarget( this._resolveRenderTarget );
		_quadMesh.material = this._resolveMaterial;
		_quadMesh.render( renderer );
		renderer.setRenderTarget( null );

		// Copy current depth to previous depth buffer
		const currentDepth = this.depthNode.value;
		if (this._previousDepthTexture.image.width !== currentDepth.width ||
			this._previousDepthTexture.image.height !== currentDepth.height ) {

			const width = currentDepth.width;
			const height = currentDepth.height;
			this.setSize( width, height );

			this._previousDepthTexture.dispose();
			this._previousDepthTexture = currentDepth.clone();
			this._previousDepthNode.value = this._previousDepthTexture;
		}
		renderer.copyTextureToTexture( currentDepth, this._previousDepthTexture );

		// restore

		RendererUtils.restoreRendererState( renderer, _rendererState );

	}

	/**
	 * This method is used to setup the effect's render targets and TSL code.
	 *
	 * @param {NodeBuilder} builder - The current node builder.
	 * @return {PassTextureNode}
	 */
	setup( builder ) {

		const postProcessing = builder.context.postProcessing;

		if ( postProcessing ) {

			this._needsPostProcessingSync = true;

			/*postProcessing.context.onBeforePostProcessing = () => {

				const size = builder.renderer.getDrawingBufferSize( _size );
				this.setViewOffset( size.width, size.height );

			};

			postProcessing.context.onAfterPostProcessing = () => {

				this.clearViewOffset();

			};*/

		}

		const depthTexture = this.depthNode;
		const velocityTexture = this.velocityNode;

		const resolve = Fn( () => {

			const uvNode = uv();

			const closestDepth = float( 1 ).toVar();
			const farthestDepth = float( 0 ).toVar();
			const closestDepthPixelPosition = vec2( 0 ).toVar();

			// sample a 3x3 neighborhood to create a box in color space
			// clamping the history color with the resulting min/max colors mitigates ghosting

			Loop( { start: int( - 1 ), end: int( 1 ), type: 'int', condition: '<=', name: 'x' }, ( { x } ) => {

				Loop( { start: int( - 1 ), end: int( 1 ), type: 'int', condition: '<=', name: 'y' }, ( { y } ) => {

					const uvNeighbor = uvNode.add( vec2( float( x ), float( y ) ).mul( this._invSize ) ).toVar();

					const currentDepth = depthTexture.sample( uvNeighbor ).r.toVar();

					// find the sample position of the closest depth in the neighborhood (used for velocity)

					If( currentDepth.lessThan( closestDepth ), () => {

						closestDepth.assign( currentDepth );
						closestDepthPixelPosition.assign( uvNeighbor );

					} );

					// find the farthest depth in the neighborhood (used to preserve edge anti-aliasing)

					If( currentDepth.greaterThan( farthestDepth ), () => {

						farthestDepth.assign( currentDepth );

					} );

				} );

			} );

			// sampling/reprojection

			const offset = velocityTexture.sample( closestDepthPixelPosition ).xy.mul( vec2( 0.5, - 0.5 ) ); // NDC to uv offset

			// calculate current frame world position

			const currentDepth = depthTexture.sample( uvNode ).r;
			const currentViewPosition = getViewPosition( uvNode, currentDepth, this._cameraProjectionMatrixInverse );
			const currentWorldPosition = this._cameraWorldMatrix.mul( vec4( currentViewPosition, 1.0 ) ).xyz;

			// calculate previous frame world position from history UV and previous depth

			const historyUV = uvNode.sub( offset );
			const previousDepth = this._previousDepthNode.sample( historyUV ).r;
			const previousViewPosition = getViewPosition( historyUV, previousDepth, this._previousCameraProjectionMatrixInverse );
			const previousWorldPosition = this._previousCameraWorldMatrix.mul( vec4( previousViewPosition, 1.0 ) ).xyz;

			// calculate difference in world positions

			const worldPositionDifference = length( currentWorldPosition.sub( previousWorldPosition ) ).toVar();
			worldPositionDifference.assign( min( max( worldPositionDifference.sub( 1.0 ), 0.0 ), 1.0 ) );

			// zero out history weight if world positions are different (indicating motion) except on edges

			const rejectPixel = worldPositionDifference.greaterThan( 0.01 ).and( farthestDepth.sub( closestDepth ).lessThan( 0.001 ) );

			return rejectPixel;

		} );

		// materials

		this._resolveMaterial.colorNode = resolve();

		return this._textureNode;

	}

	/**
	 * Frees internal resources. This method should be called
	 * when the effect is no longer required.
	 */
	dispose() {

		this._resolveRenderTarget.dispose();

		if ( this._previousDepthTexture ) {
			this._previousDepthTexture.dispose();
		}

		this._resolveMaterial.dispose();

	}

}

export default UnocclusionNode;

/**
 * TSL function for creating a Unocclusion node for regions where objects were recently unoccluded.
 *
 * @tsl
 * @function
 * @param {TextureNode} depthNode - A node that represents the scene's depth.
 * @param {TextureNode} velocityNode - A node that represents the scene's velocity.
 * @param {Camera} camera - The camera the scene is rendered with.
 * @returns {UnocclusionNode}
 */
export const unocclusion = ( depthNode, velocityNode, camera ) => nodeObject( new UnocclusionNode( depthNode, velocityNode, camera ) );
