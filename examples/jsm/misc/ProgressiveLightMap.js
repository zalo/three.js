import * as THREE from '../../../build/three.module.js';
import { potpack } from '../libs/potpack.module.js';

/**
 * Progressive Light Map Accumulator, by [zalo](https://github.com/zalo/)
 *
 * To use, simply construct a `ProgressiveLightMap` object,
 * `plmap.addObjectsToLightMap(object)` an array of semi-static
 * objects and lights to the class once, and then call
 * `plmap.update(camera)` every frame to begin accumulating
 * lighting samples.
 *
 * This should begin accumulating lightmaps which apply to
 * your objects, so you can start jittering lighting to achieve
 * the texture-space effect you're looking for.
 *
 * @param {WebGLRenderer} renderer A WebGL Rendering Context
 * @param {number} res The side-long dimension of you total lightmap
 */
class ProgressiveLightMap {

	constructor( renderer, res = 1024 ) {

		this.renderer = renderer;
		this.res = res;
		this.lightMapContainers = [];
		this.compiled = false;
		this.scene = new THREE.Scene();
		this.scene.background = null;
		this.tinyTarget = new THREE.WebGLRenderTarget( 1, 1 );
		this.buffer1Active = false;
		this.firstUpdate = true;
		this.warned = false;
		this.depthMatrix = new THREE.Matrix4().identity();
		this.cameraForward = new THREE.Vector3();

		// Create the Progressive LightMap Texture
		let format = /(Android|iPad|iPhone|iPod)/g.test( navigator.userAgent ) ? THREE.HalfFloatType : THREE.FloatType;
		this.progressiveLightMap1 = new THREE.WebGLRenderTarget( this.res, this.res, { type: format } );
		this.progressiveLightMap2 = new THREE.WebGLRenderTarget( this.res, this.res, { type: format } );

		this.stochasticDepthUVBuffer = new THREE.WebGLRenderTarget( this.res, this.res, { type: format, magFilter: THREE.NearestFilter, minFilter: THREE.NearestFilter } );
		this.progressiveBounceMap1 = new THREE.WebGLRenderTarget( this.res, this.res, { type: format } );
		this.progressiveBounceMap2 = new THREE.WebGLRenderTarget( this.res, this.res, { type: format } );

		// Inject some spicy new logic into a standard phong material
		this.uvMat = new THREE.MeshPhongMaterial();
		this.uvMat.uniforms = {};
		this.uvMat.onBeforeCompile = ( shader ) => {

			// Vertex Shader: Set Vertex Positions to the Unwrapped UV Positions
			shader.vertexShader =
				'#define USE_LIGHTMAP\n' +
				shader.vertexShader.slice( 0, - 1 ) +
				'	gl_Position = vec4((uv2 - 0.5) * 2.0, 1.0, 1.0); }';

			// Fragment Shader: Set Pixels to average in the Previous Frame's Shadows
			let bodyStart = shader.fragmentShader.indexOf( 'void main() {' );
			shader.fragmentShader =
				'varying vec2 vUv2;\n' +
				shader.fragmentShader.slice( 0, bodyStart ) +
				'	uniform sampler2D previousShadowMap;\n	uniform float averagingWindow;\n' +
				shader.fragmentShader.slice( bodyStart - 1, - 1 ) +
				`\nvec3 texelOld = texture2D(previousShadowMap, vUv2).rgb;
				gl_FragColor.rgb = mix(texelOld, gl_FragColor.rgb, 1.0/averagingWindow);
			}`;

			// Set the Previous Frame's Texture Buffer and Averaging Window
			shader.uniforms.previousShadowMap = { value: this.progressiveLightMap1.texture };
			shader.uniforms.averagingWindow = { value: 100 };

			this.uvMat.uniforms = shader.uniforms;

			// Set the new Shader to this
			this.uvMat.userData.shader = shader;

			this.compiled = true;

		};

		// This is Stochastic Order Independent Rendering for bouncing light off of layered surfaces
		this.bounceCamera = new THREE.OrthographicCamera( - 1000, 1000, 1000, - 1000, - 1000, 1000 );
		this.scene.add( this.bounceCamera );
		this.stochasticDepthUVMaterial = new THREE.ShaderMaterial( {
			vertexShader: `
				attribute vec2 uv2;
				varying vec2 vUv2;
				varying float depth;

				void main() {
					vUv2 = uv2;
					vec4 worldPosition = modelMatrix * vec4(position, 1.0);
					depth = dot(worldPosition.xyz, vec3(viewMatrix[0][2], viewMatrix[1][2], viewMatrix[2][2]));
					gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
				}
			`,
			fragmentShader: `
				varying vec2 vUv2;
				varying float depth;
				float rand(vec2 co){ // Author @patriciogv - 2015; http://patriciogonzalezvivo.com
					return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
				}
				void main() {
					gl_FragColor.rgb = vec3(vUv2, depth); 
					gl_FragColor.a = 1.0; // Extra value here, use for opacity?
					gl_FragDepth = rand(vUv2);
				}
			`,
			extensions: { fragDepth: true } // set to use fragment depth values
		} );

		this.bounceGatherMaterial = new THREE.MeshPhongMaterial();
		this.bounceGatherMaterial.uniforms = {};
		this.bounceGatherMaterial.onBeforeCompile = ( shader ) => {

			// Vertex Shader: Calculate Depth and Set Vertex Positions to the Unwrapped UV Positions
			shader.vertexShader =
				'#define USE_LIGHTMAP\nvarying vec4 vWorldPosition;\n' +
				shader.vertexShader.slice( 0, - 1 ) +
				`	vWorldPosition = worldPosition;
					gl_Position = vec4((uv2 - 0.5) * 2.0, 1.0, 1.0); }`;

			// Fragment Shader: Gather the correct appropriate bounce surface illumination
			// Then Set Pixels to average in the Previous Frame's Shadows
			let bodyStart = shader.fragmentShader.indexOf( 'void main() {' );
			shader.fragmentShader =
				'varying vec2 vUv2; varying vec4 vWorldPosition;\n' +
				shader.fragmentShader.slice( 0, bodyStart ) +
				`	uniform sampler2D previousShadowMap;
					uniform sampler2D previousBounceMap;
					uniform sampler2D stochasticDepthUVMap;
					uniform mat4 depthViewProjectionMatrix;
					uniform vec3 depthCameraDir;
					uniform vec3 depthCameraPos;
					uniform float texelStride;
					uniform float averagingWindow;
					vec2 sampleOffset[9] = vec2[9](vec2(-1, -1), vec2(0, -1), vec2( 1, -1),
												   vec2(-1,  0), vec2(0,  0), vec2( 1,  0),
												   vec2(-1,  1), vec2(0,  1), vec2( 1,  1));
					vec3 GetWorldPos(vec2 illumTexCoord, float depth) {
						return (depthCameraDir * depth) + depthCameraPos;
					}
					// From "Approximate Radiosity using Stochastic Depth Buffering" by Andreas Thomsen and Kasper Høy Nielsen
					vec3 CalculateIndirectLight(vec4 vWorldPosition, vec3 normal) {
						float weight = dot(normal, depthCameraDir);
						if (weight <= 0.0) return vec3(0,0,0);
						vec4 bestSample = vec4(0,0,0, 100000.0); //SkyColorInDirection(d)
						vec2 illumPos = (depthViewProjectionMatrix * vWorldPosition).xy;
						vec2 illumTexCoord = vec2(0,0);
						vec4 illumSample = vec4(0,0,0,0);
						vec3 samplePos = vec3(0,0,0);
						#pragma unroll_loop_start
						for (int i=0; i < 9; i++) {
							illumTexCoord = illumPos + (sampleOffset[i] * texelStride);
							illumSample = texture2D(stochasticDepthUVMap, illumTexCoord);
							samplePos = GetWorldPos(illumTexCoord, illumSample.w);
							if (dot(samplePos-vWorldPosition.xyz, normal) > 0.0 && illumSample.w < bestSample.w) {
								bestSample = texture2D(previousShadowMap, illumSample.xy);
							}
						}
						#pragma unroll_loop_end
						return 4.0 * bestSample.rgb * weight;
					}
					` +
					shader.fragmentShader.slice( bodyStart - 1, - 1 ) +
					`
					gl_FragColor.rgb = CalculateIndirectLight(vWorldPosition, normal);

					vec3 texelOld = texture2D(previousBounceMap, vUv2).rgb;
					gl_FragColor.rgb = mix(texelOld, gl_FragColor.rgb, 1.0/averagingWindow);}`;

			// Set the Previous Frame's Texture Buffer and Averaging Window
			shader.uniforms.previousShadowMap = { value: this.progressiveLightMap2.texture };
			shader.uniforms.previousBounceMap = { value: this.progressiveBounceMap2.texture };
			shader.uniforms.stochasticDepthUVMap = { value: this.stochasticDepthUVBuffer.texture };
			shader.uniforms.depthViewProjectionMatrix = { value: this.depthMatrix };
			shader.uniforms.depthCameraDir = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorldInverse, 2 ) };
			shader.uniforms.depthCameraPos = { value: this.bounceCamera.position };
			shader.uniforms.averagingWindow = { value: 100 };
			shader.uniforms.texelStride = { value: 1.0 / this.res };

			this.bounceGatherMaterial.uniforms = shader.uniforms;

			// Set the new Shader to this
			this.bounceGatherMaterial.userData.shader = shader;

			this.compiled = true;

		};

	}

	/**
	 * Sets these objects' materials' lightmaps and modifies their uv2's.
	 * @param {Object3D} objects An array of objects and lights to set up your lightmap.
	 */
	addObjectsToLightMap( objects ) {

		// Prepare list of UV bounding boxes for packing later...
		this.uv_boxes = []; let padding = 3 / this.res;

		for ( let ob = 0; ob < objects.length; ob ++ ) {

			let object = objects[ ob ];

			// If this object is a light, simply add it to the internal scene
			if ( object.isLight ) {

				this.scene.attach( object ); continue;

			}

			if ( ! object.geometry.hasAttribute( "uv" ) ) {

				console.warn( "All lightmap objects need UVs!" ); continue;

			}

			if ( this.blurringPlane == null ) {

				this._initializeBlurPlane( this.res, this.progressiveLightMap1 );

			}

			// Apply the lightmap to the object
			object.material.lightMap = this.progressiveLightMap2.texture;
			object.material.dithering = true;
			object.castShadow = true;
			object.receiveShadow = true;
			object.renderOrder = 1000 + ob;

			// Prepare UV boxes for potpack
			// TODO: Size these by object surface area
			this.uv_boxes.push( { w: 1 + ( padding * 2 ),
								  h: 1 + ( padding * 2 ), index: ob } );

			this.lightMapContainers.push( { basicMat: object.material, object: object } );

			this.compiled = false;

		}

		// Pack the objects' lightmap UVs into the same global space
		const dimensions = potpack( this.uv_boxes );
		this.uv_boxes.forEach( ( box ) => {

			let uv2 = objects[ box.index ].geometry.getAttribute( "uv" ).clone();
			for ( let i = 0; i < uv2.array.length; i += uv2.itemSize ) {

				uv2.array[ i ] = ( uv2.array[ i ] + box.x + padding ) / dimensions.w;
				uv2.array[ i + 1 ] = ( uv2.array[ i + 1 ] + box.y + padding ) / dimensions.h;

			}

			objects[ box.index ].geometry.setAttribute( "uv2", uv2 );
			objects[ box.index ].geometry.getAttribute( "uv2" ).needsUpdate = true;

		} );

	}

	/**
	 * This function renders each mesh one at a time into their respective surface maps
	 * @param {Camera} camera Standard Rendering Camera
	 * @param {number} blendWindow When >1, samples will accumulate over time.
	 * @param {boolean} blurEdges  Whether to fix UV Edges via blurring
	 */
	update( camera, blendWindow = 100, blurEdges = true ) {

		if ( this.blurringPlane == null ) {

			return;

		}

		// Store the original Render Target
		let oldTarget = this.renderer.getRenderTarget();

		// The blurring plane applies blur to the seams of the lightmap
		this.blurringPlane.visible = blurEdges;

		// Steal the Object3D from the real world to our special dimension
		for ( let l = 0; l < this.lightMapContainers.length; l ++ ) {

			this.lightMapContainers[ l ].object.oldScene =
				this.lightMapContainers[ l ].object.parent;
			this.scene.attach( this.lightMapContainers[ l ].object );

		}

		// Render once normally to initialize everything
		if ( this.firstUpdate ) {

			this.renderer.setRenderTarget( this.tinyTarget ); // Tiny for Speed
			this.renderer.render( this.scene, camera );
			this.firstUpdate = false;

		}

		// Set each object's material to the UV Unwrapped Surface Mapping Version
		for ( let l = 0; l < this.lightMapContainers.length; l ++ ) {

			this.uvMat.uniforms.averagingWindow = { value: blendWindow };
			this.lightMapContainers[ l ].object.material = this.uvMat;
			this.lightMapContainers[ l ].object.oldFrustumCulled =
				this.lightMapContainers[ l ].object.frustumCulled;
			this.lightMapContainers[ l ].object.frustumCulled = false;

		}

		// Ping-pong two surface buffers for reading/writing
		let activeMap = this.buffer1Active ? this.progressiveLightMap1 : this.progressiveLightMap2;
		let inactiveMap = this.buffer1Active ? this.progressiveLightMap2 : this.progressiveLightMap1;

		// Render the object's surface maps
		this.renderer.setRenderTarget( activeMap );
		this.uvMat.uniforms.previousShadowMap = { value: inactiveMap.texture };
		this.blurringPlane.material.uniforms.previousShadowMap = { value: inactiveMap.texture };
		this.buffer1Active = ! this.buffer1Active;
		this.renderer.render( this.scene, camera );

		// BEGIN INDIRECT BOUNCE PHASE
		// Set each object's material to the stochastic depth version
		for ( let l = 0; l < this.lightMapContainers.length; l ++ ) {

			this.lightMapContainers[ l ].object.material = this.stochasticDepthUVMaterial;

		}

		// Render the stochastic depth buffer from a random directional camera's perspective
		this.renderer.setRenderTarget( this.stochasticDepthUVBuffer );
		this.renderer.render( this.scene, this.bounceCamera );

		// Update the depth camera matrix
		this.depthMatrix.set(
			0.5, 0.0, 0.0, 0.5,
			0.0, 0.5, 0.0, 0.5,
			0.0, 0.0, 0.5, 0.5,
			0.0, 0.0, 0.0, 1.0 );
		this.depthMatrix.multiply( this.bounceCamera.projectionMatrix );
		this.depthMatrix.multiply( this.bounceCamera.matrixWorldInverse );

		// Ping-pong two surface buffers for reading/writing
		let activeBounceMap = this.buffer1Active ? this.progressiveBounceMap1 : this.progressiveBounceMap2;
		let inactiveBounceMap = this.buffer1Active ? this.progressiveBounceMap2 : this.progressiveBounceMap1;

		this.bounceGatherMaterial.uniforms.previousShadowMap = { value: inactiveMap.texture };
		this.bounceGatherMaterial.uniforms.previousBounceMap = { value: inactiveBounceMap.texture };
		this.bounceGatherMaterial.uniforms.stochasticDepthUVMap = { value: this.stochasticDepthUVBuffer.texture };
		this.bounceGatherMaterial.uniforms.depthViewProjectionMatrix = { value: this.depthMatrix };
		this.bounceGatherMaterial.uniforms.depthCameraDir = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorldInverse, 2 ) };
		this.bounceGatherMaterial.uniforms.depthCameraPos = { value: this.bounceCamera.position };
		this.bounceGatherMaterial.uniforms.averagingWindow = { value: 100 };
		this.bounceGatherMaterial.uniforms.texelStride = { value: 1.0 / this.res };
		this.bounceGatherMaterial.needsUpdate = true;

		// Set each object's material to the bounce gathering material
		for ( let l = 0; l < this.lightMapContainers.length; l ++ ) {

			this.lightMapContainers[ l ].object.material = this.bounceGatherMaterial;

		}

		// Gather the bounce
		this.renderer.setRenderTarget( activeBounceMap );
		this.renderer.render( this.scene, this.bounceCamera );

		// END INDIRECT BOUNCE PHASE

		// Restore the object's Real-time Material and add it back to the original world
		for ( let l = 0; l < this.lightMapContainers.length; l ++ ) {

			this.lightMapContainers[ l ].object.frustumCulled =
				this.lightMapContainers[ l ].object.oldFrustumCulled;
			this.lightMapContainers[ l ].object.material = this.lightMapContainers[ l ].basicMat;
			this.lightMapContainers[ l ].object.oldScene.attach( this.lightMapContainers[ l ].object );

		}

		// Restore the original Render Target
		this.renderer.setRenderTarget( oldTarget );

	}

	/** DEBUG
	 * Draw the lightmap in the main scene.  Call this after adding the objects to it.
	 * @param {boolean} visible Whether the debug plane should be visible
	 * @param {Vector3} position Where the debug plane should be drawn
	*/
	showDebugLightmap( visible, position = undefined ) {

		if ( this.lightMapContainers.length == 0 ) {

			if ( ! this.warned ) {

				console.warn( "Call this after adding the objects!" ); this.warned = true;

			}

			return;

		}

		if ( this.labelMesh == null ) {

			this.labelMaterial = new THREE.MeshBasicMaterial(
				{ map: this.progressiveLightMap1.texture, side: THREE.DoubleSide } );
			this.labelPlane = new THREE.PlaneGeometry( 100, 100 );
			this.labelMesh = new THREE.Mesh( this.labelPlane, this.labelMaterial );
			this.labelMesh.position.y = 250;
			this.lightMapContainers[ 0 ].object.parent.add( this.labelMesh );

		}

		if ( position != undefined ) {

			this.labelMesh.position.copy( position );

		}

		this.labelMesh.visible = visible;

	}

	/**
	 * INTERNAL Creates the Blurring Plane
	 * @param {number} res The square resolution of this object's lightMap.
	 * @param {WebGLRenderTexture} lightMap The lightmap to initialize the plane with.
	 */
	_initializeBlurPlane( res, lightMap = null ) {

		let blurMaterial = new THREE.MeshBasicMaterial();
		blurMaterial.uniforms = { previousShadowMap: { value: null },
								  pixelOffset: { value: 1.0 / res },
								  polygonOffset: true, polygonOffsetFactor: - 1, polygonOffsetUnits: 3.0 };
		blurMaterial.onBeforeCompile = ( shader ) => {

			// Vertex Shader: Set Vertex Positions to the Unwrapped UV Positions
			shader.vertexShader =
				'#define USE_UV\n' +
				shader.vertexShader.slice( 0, - 1 ) +
				'	gl_Position = vec4((uv - 0.5) * 2.0, 1.0, 1.0); }';

			// Fragment Shader: Set Pixels to 9-tap box blur the current frame's Shadows
			let bodyStart	= shader.fragmentShader.indexOf( 'void main() {' );
			shader.fragmentShader =
				'#define USE_UV\n' +
				shader.fragmentShader.slice( 0, bodyStart ) +
				'	uniform sampler2D previousShadowMap;\n	uniform float pixelOffset;\n' +
				shader.fragmentShader.slice( bodyStart - 1, - 1 ) +
					`	gl_FragColor.rgb = (
									texture2D(previousShadowMap, vUv + vec2( pixelOffset,  0.0        )).rgb + 
									texture2D(previousShadowMap, vUv + vec2( 0.0        ,  pixelOffset)).rgb +
									texture2D(previousShadowMap, vUv + vec2( 0.0        , -pixelOffset)).rgb +
									texture2D(previousShadowMap, vUv + vec2(-pixelOffset,  0.0        )).rgb +
									texture2D(previousShadowMap, vUv + vec2( pixelOffset,  pixelOffset)).rgb + 
									texture2D(previousShadowMap, vUv + vec2(-pixelOffset,  pixelOffset)).rgb +
									texture2D(previousShadowMap, vUv + vec2( pixelOffset, -pixelOffset)).rgb +
									texture2D(previousShadowMap, vUv + vec2(-pixelOffset, -pixelOffset)).rgb)/8.0;
				}`;

			// Set the LightMap Accumulation Buffer
			shader.uniforms.previousShadowMap = { value: lightMap.texture };
			shader.uniforms.pixelOffset = { value: 0.5 / res };
			blurMaterial.uniforms = shader.uniforms;

			// Set the new Shader to this
			blurMaterial.userData.shader = shader;

			this.compiled = true;

		};

		this.blurringPlane = new THREE.Mesh( new THREE.PlaneBufferGeometry( 1, 1 ), blurMaterial );
		this.blurringPlane.name = "Blurring Plane";
		this.blurringPlane.frustumCulled = false;
		this.blurringPlane.renderOrder = 0;
		this.blurringPlane.material.depthWrite = false;
		this.scene.add( this.blurringPlane );

	}

}

export { ProgressiveLightMap };
