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
		this.lights = [];
		this.numSamples = 1.0;
		this.switchingNum = 5000;

		// Create the Progressive LightMap Texture
		let format = /(Android|iPad|iPhone|iPod)/g.test( navigator.userAgent ) ? THREE.HalfFloatType : THREE.FloatType;
		this.progressiveLightMap1 = new THREE.WebGLRenderTarget( this.res, this.res, { type: format } );
		this.progressiveLightMap2 = new THREE.WebGLRenderTarget( this.res, this.res, { type: format } );

		this.stochasticDepthColorBuffer = new THREE.WebGLRenderTarget( this.res / 2.0, this.res / 2.0, { type: format, magFilter: THREE.NearestFilter, minFilter: THREE.NearestFilter } );
		this.progressiveBounceMap1 = new THREE.WebGLRenderTarget( this.res / 2.0, this.res / 2.0, { type: format } );
		this.progressiveBounceMap2 = new THREE.WebGLRenderTarget( this.res / 2.0, this.res / 2.0, { type: format } );

		this.compositeLightMap1 = new THREE.WebGLRenderTarget( this.res, this.res, { type: format } );
		this.compositeLightMap2 = new THREE.WebGLRenderTarget( this.res, this.res, { type: format } );

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
				`	uniform sampler2D previousShadowMap;
					uniform sampler2D previousBounceMap;
					uniform float averagingWindow;
					` +
				shader.fragmentShader.slice( bodyStart - 1, - 1 ) +
				`
				vec3 texelOld = texture2D(previousShadowMap, vUv2).rgb;
				//vec3 bounceTexelOld = texture2D(previousBounceMap, vUv2).rgb;
				gl_FragColor.rgb = mix(texelOld, gl_FragColor.rgb, 1.0/averagingWindow);
			}`;

			// Set the Previous Frame's Texture Buffer and Averaging Window
			shader.uniforms.previousShadowMap = { value: this.progressiveLightMap1.texture };
			shader.uniforms.previousBounceMap = { value: this.progressiveBounceMap1.texture };
			shader.uniforms.averagingWindow = { value: 100 };

			this.uvMat.uniforms = shader.uniforms;

			// Set the new Shader to this
			this.uvMat.userData.shader = shader;

			this.compiled = true;

		};

		// This is Stochastic Order Independent Rendering for bouncing light off of layered surfaces
		this.bounceCamera = new THREE.OrthographicCamera( - 300, 300, 300, - 300, - 1000, 1000 );
		this.scene.add( this.bounceCamera );

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
					uniform vec3 depthCameraX;
					uniform vec3 depthCameraY;
					uniform vec3 depthCameraDir;
					uniform vec3 depthCameraPos;
					uniform float texelStride;
					uniform float switchingNum;
					uniform float numSamples;
					uniform vec2 sampleOffset[9];
					vec3 GetWorldPos(vec2 illumTexCoord, float depth) {
						return (depthCameraDir * depth) +
							   ((600.0 * illumTexCoord.x - 300.0) * depthCameraX) +
							   ((600.0 * illumTexCoord.y - 300.0) * depthCameraY);
					}
					// From "Approximate Radiosity using Stochastic Depth Buffering" by Andreas Thomsen and Kasper Høy Nielsen
					vec3 CalculateIndirectLight(vec4 vWorldPosition, vec3 normal) {
						float weight = dot(normal, depthCameraDir);
						if (weight >= 0.0) return vec3(0,0,0);
						vec4 bestSample = vec4(0, 0.0, 0.0, -100000.0); //SkyColorInDirection(d)
						vec2 illumPos = (depthViewProjectionMatrix * vWorldPosition).xy;
						vec2 illumTexCoord = vec2(0,0);
						vec4 illumSample = vec4(0,0,0,0);//texture2D(stochasticDepthUVMap, illumPos);
						vec3 samplePos = vec3(0,0,0);
						#pragma unroll_loop_start
						for (int i = 0; i < 9; i++) {
							illumTexCoord = illumPos + (sampleOffset[i] * texelStride);
							illumSample = texture2D(stochasticDepthUVMap, illumTexCoord);
							samplePos = GetWorldPos(illumTexCoord, illumSample.w);
							if (dot(samplePos-vWorldPosition.xyz, normal) > 0.0 && illumSample.w > bestSample.w) {
								bestSample = illumSample;
							}
						}
						#pragma unroll_loop_end
						return 4.0 * bestSample.rgb * -weight;
					}
					` +
					shader.fragmentShader.slice( bodyStart - 1, - 1 ) +
					`
					vec3 worldNormal = inverseTransformDirection( normal, viewMatrix );
					gl_FragColor.rgb = CalculateIndirectLight(vWorldPosition, worldNormal);

					vec3 texelOld = texture2D(previousBounceMap, vUv2).rgb;
					if(numSamples >= switchingNum) {
						gl_FragColor.rgb += mix(texelOld, gl_FragColor.rgb, 1.0/switchingNum);
					} else {
						gl_FragColor.rgb += texelOld;
					}
				}`;

			// Set the Previous Frame's Texture Buffer and Averaging Window
			shader.uniforms.previousShadowMap = { value: this.progressiveLightMap2.texture };
			shader.uniforms.previousBounceMap = { value: this.progressiveBounceMap2.texture };
			shader.uniforms.stochasticDepthUVMap = { value: this.stochasticDepthColorBuffer.texture };
			shader.uniforms.depthViewProjectionMatrix = { value: this.depthMatrix };
			shader.uniforms.depthCameraX = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorld, 0 ).multiplyScalar( - 1.0 ).clone() };
			shader.uniforms.depthCameraY = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorld, 1 ).multiplyScalar( - 1.0 ).clone() };
			shader.uniforms.depthCameraDir = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorld, 2 ).clone() };
			shader.uniforms.depthCameraPos = { value: this.bounceCamera.position };
			shader.uniforms.switchingNum = { value: this.switchingNum };
			shader.uniforms.numSamples = { value: this.numSamples };
			shader.uniforms.texelStride = { value: 1.0 / this.res };
			shader.uniforms.sampleOffset = { value: [ new THREE.Vector2( - 1, - 1 ),
													  new THREE.Vector2( 0, - 1 ),
													  new THREE.Vector2( 1, - 1 ),
													  new THREE.Vector2( - 1, 0 ),
													  new THREE.Vector2( 0, 0 ),
													  new THREE.Vector2( 1, 0 ),
													  new THREE.Vector2( - 1, 1 ),
													  new THREE.Vector2( 0, 1 ),
													  new THREE.Vector2( 1, 1 ) ] };


			this.bounceGatherMaterial.uniforms = shader.uniforms;

			// Set the new Shader to this
			this.bounceGatherMaterial.userData.shader = shader;

			this.compiled = true;

		};

		this.compositeDirectAndIndirectMaterial = new THREE.ShaderMaterial( {
			vertexShader: `
				attribute vec2 uv2;
				varying vec2 vUv2;
				void main() {
					vUv2 = uv2;
					gl_Position = vec4((uv2 - 0.5) * 2.0, 1.0, 1.0);
				}
			`,
			fragmentShader: `
				varying vec2 vUv2;
				uniform sampler2D previousShadowMap;
				uniform sampler2D previousBounceMap;
				uniform float indirectContribution;
				void main() {
					gl_FragColor =   texture2D(previousShadowMap, vUv2) +
									(texture2D(previousBounceMap, vUv2) * indirectContribution);
				}
			`,
			uniforms: {
				previousShadowMap: { value: this.progressiveLightMap2.texture },
				previousBounceMap: { value: this.progressiveBounceMap2.texture }
			}
		} );

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

			//if ( ob == objects.length - 1 ) {

			//	let helper = new THREE.CameraHelper( this.bounceCamera );
			//	object.parent.add( helper );

			//}

			// If this object is a light, simply add it to the internal scene
			if ( object.isLight ) {

				object.shadow.autoUpdate = false;

				this.lights.push( object );

				this.scene.attach( object ); continue;

			}

			if ( ! object.geometry.hasAttribute( "uv" ) ) {

				console.warn( "All lightmap objects need UVs!" ); continue;

			}

			if ( this.blurringPlane == null ) {

				this._initializeBlurPlane( this.res, this.progressiveLightMap1 );

			}

			// Inject some spicy stochastic depth logic into this object's material
			let stochasticDepthMaterial = object.material.clone();
			stochasticDepthMaterial.uniforms = {};
			stochasticDepthMaterial.extensions = { fragDepth: true }; // set to use fragment depth values
			stochasticDepthMaterial.onBeforeCompile = ( shader ) => {

				// Vertex Shader: Set Vertex Positions to the Unwrapped UV Positions
				shader.vertexShader =
					`#define USE_LIGHTMAP
					# define USE_SHADOWMAP
					varying float depth;
					uniform vec3 depthCameraDir;
					//varying vec2 vUv2;
					` +
					shader.vertexShader.slice( 0, - 1 ) +
					`	//vUv2 = uv2;
						depth = dot(worldPosition.xyz, depthCameraDir); }`;

				// Fragment Shader: Set Pixels to average in the Previous Frame's Shadows
				let bodyStart = shader.fragmentShader.indexOf( 'void main() {' );
				shader.fragmentShader =
					shader.fragmentShader.slice( 0, bodyStart ) +
					`	uniform sampler2D previousShadowMap;
						varying float depth;
						uniform float time;
						varying vec2 vUv2;
						float randd(vec2 co){ // Author @patriciogv - 2015; http://patriciogonzalezvivo.com
							return fract(sin(dot(co.xy ,vec2(12.9898,78.233) + time)) * 43758.5453);
						}
						` +
					shader.fragmentShader.slice( bodyStart - 1, - 1 ) +
					`
						gl_FragColor.a = depth; // Extra value here, use for opacity?
						gl_FragDepthEXT = (randd(vUv2) * 20.0)-10.0;
					}`;

				// Set the Previous Frame's Texture Buffer and Averaging Window
				shader.uniforms.previousShadowMap = { value: this.progressiveLightMap1.texture };
				shader.uniforms.time = { value: performance.now() };
				shader.uniforms.depthCameraDir = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorld, 2 ).clone() };

				stochasticDepthMaterial.uniforms = shader.uniforms;

				// Set the new Shader to this
				stochasticDepthMaterial.userData.shader = shader;

				this.compiled = true;

			};

			// Apply the lightmap to the object
			object.material.dithering = true;
			//object.castShadow = true;
			object.receiveShadow = true;
			object.renderOrder = 1000 + ob;

			// Prepare UV boxes for potpack
			if ( ! object.excludeFromLightMap ) {

				// Determine the true surface area of this mesh as a ratio
				// of world surface area to used UV surface area
				let surfaceAreaSqrt = Math.sqrt( this._findSurfaceArea( object, false ) /
												 this._findSurfaceArea( object, true ) );

				this.uv_boxes.push( { w: surfaceAreaSqrt * ( 1 + ( padding * 2 ) ),
									  h: surfaceAreaSqrt * ( 1 + ( padding * 2 ) ),
									  index: ob, surfaceAreaSqrt: surfaceAreaSqrt } );
				object.material.lightMap = this.compositeLightMap1.texture;

			}

			this.lightMapContainers.push( {
				depthMat: stochasticDepthMaterial,
				basicMat: object.material,
				object: object
			} );

			this.compiled = false;

		}

		// Pack the objects' lightmap UVs into the same global space
		const dimensions = potpack( this.uv_boxes );
		this.uv_boxes.forEach( ( box ) => {

			let uv2 = objects[ box.index ].geometry.getAttribute( "uv" ).clone();
			for ( let i = 0; i < uv2.array.length; i += uv2.itemSize ) {

				uv2.array[ i ] = ( ( ( uv2.array[ i ] * box.surfaceAreaSqrt ) ) + box.x + padding ) / dimensions.w;
				uv2.array[ i + 1 ] = ( ( uv2.array[ i + 1 ] * box.surfaceAreaSqrt ) + box.y + padding ) / dimensions.h;

			}

			objects[ box.index ].geometry.setAttribute( "uv2", uv2 );
			objects[ box.index ].geometry.getAttribute( "uv2" ).needsUpdate = true;

		} );

	}

	/**
	 * INTERNAL: This function returns the surface area of the input object's geometry as a float
	 * @param {Object3D} camera Standard Rendering Camera
	 * @param {Boolean} uvs Whether to calculate the world surface area, or the used UV surface area.
	 */
	_findSurfaceArea( object, uvs = true ) {

		object.updateWorldMatrix( true, false );
		let sum = 0;
		let tris = object.geometry.index.array;
		let vertices = object.geometry.getAttribute( uvs ? "uv" : "position" );
		let v1 = new THREE.Vector3( 0, 0, 0 ),
			v2 = new THREE.Vector3( 0, 0, 0 ),
			v3 = new THREE.Vector3( 0, 0, 0 );
		let uv1 = new THREE.Vector2( 0, 0 ),
			uv2 = new THREE.Vector2( 0, 0 ),
			uv3 = new THREE.Vector2( 0, 0 );

		for ( let t = 0; t < tris.length; t += 3 ) {

			if ( ! uvs ) {

				v1.fromBufferAttribute( vertices, tris[ t + 0 ] );
				v2.fromBufferAttribute( vertices, tris[ t + 1 ] );
				v3.fromBufferAttribute( vertices, tris[ t + 2 ] );
				object.localToWorld( v1 ); // Multiply by matrixWorld for true size
				object.localToWorld( v2 );
				object.localToWorld( v3 );

			} else {

				uv1.fromBufferAttribute( vertices, tris[ t + 0 ] ); v1.set( uv1.x, uv1.y, 0 );
				uv2.fromBufferAttribute( vertices, tris[ t + 1 ] ); v2.set( uv2.x, uv2.y, 0 );
				uv3.fromBufferAttribute( vertices, tris[ t + 2 ] ); v3.set( uv3.x, uv3.y, 0 );

			}

			// Calculate the area of this triangle
			v3.sub( v2 ); v1.sub( v2 );
			sum += v3.cross( v1 ).length() * 0.5;

		}

		console.log( sum );

		return sum;

	}

	/**
	 * This function renders each mesh one at a time into their respective surface maps
	 * @param {Camera} camera Standard Rendering Camera
	 * @param {number} blendWindow When >1, samples will accumulate over time.
	 * @param {boolean} blurEdges  Whether to fix UV Edges via blurring
	 */
	update( camera, blendWindow = 100, blurEdges = true, indirectContribution = 1.0, indirectBlendWindow = 5000 ) {

		if ( this.blurringPlane == null ) {

			return;

		}

		// Store the original Render Target
		let oldTarget = this.renderer.getRenderTarget();

		// The blurring plane applies blur to the seams of the lightmap
		this.blurringPlane.visible = blurEdges;

		// Steal the Object3D from the real world to our special dimension
		for ( let l = 0; l < this.lights.length; l ++ ) {

			this.lights[ l ].shadow.needsUpdate = true;

		}

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
			this.lightMapContainers[ l ].object.visible = ! this.lightMapContainers[ l ].object.excludeFromLightMap;

		}

		// Ping-pong two surface buffers for reading/writing
		let activeMap = this.buffer1Active ? this.progressiveLightMap1 : this.progressiveLightMap2;
		let inactiveMap = this.buffer1Active ? this.progressiveLightMap2 : this.progressiveLightMap1;
		//let activeBounceMap = this.buffer1Active ? this.progressiveBounceMap1 : this.progressiveBounceMap2;
		let inactiveBounceMap = this.buffer1Active ? this.progressiveBounceMap2 : this.progressiveBounceMap1;

		// Render the object's surface maps
		this.renderer.setRenderTarget( activeMap );
		this.uvMat.uniforms.previousShadowMap = { value: inactiveMap.texture };
		this.uvMat.uniforms.previousBounceMap = { value: inactiveBounceMap.texture };
		this.blurringPlane.material.uniforms.previousShadowMap = { value: inactiveMap.texture };

		this.renderer.render( this.scene, camera );

		// BEGIN INDIRECT BOUNCE PHASE
		if ( indirectContribution > 0 ) {

			for ( let bounceIter = 0; bounceIter < 1 + ( 3 * 2 ); bounceIter ++ ) {

				this.buffer1Active = ! this.buffer1Active;
				this.blurringPlane.visible = false;

				// Set each object's material to the stochastic depth version
				for ( let l = 0; l < this.lightMapContainers.length; l ++ ) {

					this.lightMapContainers[ l ].depthMat.uniforms.time = { value: performance.now() + bounceIter };
					this.lightMapContainers[ l ].depthMat.uniforms.depthCameraDir = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorld, 2 ).clone() };
					this.lightMapContainers[ l ].depthMat.needsUpdate = true;
					this.lightMapContainers[ l ].object.material = this.lightMapContainers[ l ].depthMat;
					this.lightMapContainers[ l ].object.visible = true;

				}

				// Render the stochastic depth buffer from a random directional camera's perspective
				this.renderer.setRenderTarget( this.stochasticDepthColorBuffer );
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
				this.bounceGatherMaterial.uniforms.stochasticDepthUVMap = { value: this.stochasticDepthColorBuffer.texture };
				this.bounceGatherMaterial.uniforms.depthViewProjectionMatrix = { value: this.depthMatrix };
				this.bounceGatherMaterial.uniforms.depthCameraX = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorld, 0 ).normalize().clone() };
				this.bounceGatherMaterial.uniforms.depthCameraY = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorld, 1 ).normalize().clone() };
				this.bounceGatherMaterial.uniforms.depthCameraDir = { value: this.cameraForward.setFromMatrixColumn( this.bounceCamera.matrixWorld, 2 ).clone() };
				this.bounceGatherMaterial.uniforms.depthCameraPos = { value: new THREE.Vector3() };// "Pos" is whatever the center of the frustum is//this.bounceCamera.position };
				this.bounceGatherMaterial.uniforms.averagingWindow = { value: blendWindow };
				this.bounceGatherMaterial.uniforms.texelStride = { value: 1.0 / this.res };
				this.bounceGatherMaterial.uniforms.switchingNum = { value: indirectBlendWindow };
				this.bounceGatherMaterial.uniforms.numSamples = { value: this.numSamples };
				this.bounceGatherMaterial.needsUpdate = true;
				this.blurringPlane.visible = true;
				this.blurringPlane.material.uniforms.previousShadowMap = { value: inactiveBounceMap.texture };

				// Set each object's material to the bounce gathering material
				for ( let l = 0; l < this.lightMapContainers.length; l ++ ) {

					this.lightMapContainers[ l ].object.material = this.bounceGatherMaterial;
					this.lightMapContainers[ l ].object.visible = ! this.lightMapContainers[ l ].object.excludeFromLightMap;

				}

				// Uniform Hemispherical Surface Distribution for Ambient Occlusion
				let lambda = Math.acos( 2 * Math.random() - 1 ) - ( 3.14159 / 2.0 );
				let phi = 2 * 3.14159 * Math.random();
				this.bounceCamera.position.set( ( ( Math.cos( lambda ) * Math.cos( phi ) ) * 300 ),
					( ( Math.cos( lambda ) * Math.sin( phi ) ) * 300 ) + 20,
					( Math.sin( lambda ) * 300 ) );
				//this.bounceCamera.position.set( 300, 300, 300 );
				this.bounceCamera.lookAt( 0, 0, 0 );
				this.bounceCamera.updateProjectionMatrix();

				// Gather the bounce
				this.renderer.setRenderTarget( activeBounceMap );
				this.renderer.render( this.scene, this.bounceCamera );

				this.numSamples = Math.min( indirectBlendWindow, this.numSamples + 1.0 );

			}
			// END INDIRECT BOUNCE PHASE

		}

		let activeCompositeMap = this.buffer1Active ? this.compositeLightMap1 : this.compositeLightMap2;
		let inactiveCompositeMap = this.buffer1Active ? this.compositeLightMap2 : this.compositeLightMap1;
		this.blurringPlane.visible = blurEdges;
		this.blurringPlane.material.uniforms.previousShadowMap = { value: inactiveCompositeMap.texture };
		this.compositeDirectAndIndirectMaterial.uniforms.previousShadowMap = { value: this.progressiveLightMap1.texture };
		this.compositeDirectAndIndirectMaterial.uniforms.previousBounceMap = { value: this.progressiveBounceMap1.texture };
		this.compositeDirectAndIndirectMaterial.uniforms.indirectContribution = { value: indirectContribution / ( this.numSamples ) };

		// Composite the direct and Indirect Lighting Steps together
		for ( let l = 0; l < this.lightMapContainers.length; l ++ ) {

			this.lightMapContainers[ l ].object.material = this.compositeDirectAndIndirectMaterial;
			this.lightMapContainers[ l ].object.visible = ! this.lightMapContainers[ l ].object.excludeFromLightMap;

		}

		// Gather the bounce
		this.renderer.setRenderTarget( activeCompositeMap );
		this.renderer.render( this.scene, camera );

		// Restore the object's Real-time Material and add it back to the original world
		for ( let l = 0; l < this.lightMapContainers.length; l ++ ) {

			this.lightMapContainers[ l ].object.frustumCulled =
				this.lightMapContainers[ l ].object.oldFrustumCulled;

			this.lightMapContainers[ l ].object.material = this.lightMapContainers[ l ].basicMat;

			this.lightMapContainers[ l ].object.oldScene.attach( this.lightMapContainers[ l ].object );
			this.lightMapContainers[ l ].object.visible = true;


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
				{ map: this.compositeLightMap1.texture, side: THREE.DoubleSide } );
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
