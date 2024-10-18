import {
	AddEquation,
	Color,
	CustomBlending,
	DataTexture,
	DepthTexture,
	DepthStencilFormat,
	DstAlphaFactor,
	DstColorFactor,
	HalfFloatType,
	MeshNormalMaterial,
	NearestFilter,
	NoBlending,
	RepeatWrapping,
	RGBAFormat,
	ShaderMaterial,
	UniformsUtils,
	UnsignedByteType,
	UnsignedInt248Type,
	WebGLRenderTarget,
	ZeroFactor
} from 'three';
import { Pass, FullScreenQuad } from './Pass.js';
import { generateMagicSquareNoise, SSILVBShader, SSILVBDepthShader, SSILVBBlendShader } from '../shaders/SSILVBShader.js';
import { generatePdSamplePointInitializer, PoissonDenoiseShader } from '../shaders/PoissonDenoiseShader.js';
import { CopyShader } from '../shaders/CopyShader.js';
import { SimplexNoise } from '../math/SimplexNoise.js';

class SSILVBPass extends Pass {

	constructor( scene, camera, width, height, parameters, aoParameters, pdParameters ) {

		super();

		this.width = ( width !== undefined ) ? width : 512;
		this.height = ( height !== undefined ) ? height : 512;
		this.clear = true;
		this.camera = camera;
		this.scene = scene;
		this.output = 0;
		this._renderGBuffer = true;
		this._visibilityCache = new Map();
		this.blendIntensity = 1.;

		this.pdRings = 2.;
		this.pdRadiusExponent = 2.;
		this.pdSamples = 16;

		this.ssilvbNoiseTexture = generateMagicSquareNoise();
		this.pdNoiseTexture = this.generateNoise();

		this.ssilvbRenderTarget = new WebGLRenderTarget( this.width, this.height, { type: HalfFloatType } );
		this.pdRenderTarget = this.ssilvbRenderTarget.clone();

		this.ssilvbMaterial = new ShaderMaterial( {
			defines: Object.assign( {}, SSILVBShader.defines ),
			uniforms: UniformsUtils.clone( SSILVBShader.uniforms ),
			vertexShader: SSILVBShader.vertexShader,
			fragmentShader: SSILVBShader.fragmentShader,
			blending: NoBlending,
			depthTest: false,
			depthWrite: false,
		} );
		this.ssilvbMaterial.defines.PERSPECTIVE_CAMERA = this.camera.isPerspectiveCamera ? 1 : 0;
		this.ssilvbMaterial.uniforms.tNoise.value = this.ssilvbNoiseTexture;
		this.ssilvbMaterial.uniforms.resolution.value.set( this.width, this.height );
		this.ssilvbMaterial.uniforms.cameraNear.value = this.camera.near;
		this.ssilvbMaterial.uniforms.cameraFar.value = this.camera.far;

		this.normalMaterial = new MeshNormalMaterial();
		this.normalMaterial.blending = NoBlending;

		this.pdMaterial = new ShaderMaterial( {
			defines: Object.assign( {}, PoissonDenoiseShader.defines ),
			uniforms: UniformsUtils.clone( PoissonDenoiseShader.uniforms ),
			vertexShader: PoissonDenoiseShader.vertexShader,
			fragmentShader: PoissonDenoiseShader.fragmentShader,
			depthTest: false,
			depthWrite: false,
		} );
		this.pdMaterial.uniforms.tDiffuse.value = this.ssilvbRenderTarget.texture;
		this.pdMaterial.uniforms.tNoise.value = this.pdNoiseTexture;
		this.pdMaterial.uniforms.resolution.value.set( this.width, this.height );
		this.pdMaterial.uniforms.lumaPhi.value = 10;
		this.pdMaterial.uniforms.depthPhi.value = 2;
		this.pdMaterial.uniforms.normalPhi.value = 3;
		this.pdMaterial.uniforms.radius.value = 8;

		this.depthRenderMaterial = new ShaderMaterial( {
			defines: Object.assign( {}, SSILVBDepthShader.defines ),
			uniforms: UniformsUtils.clone( SSILVBDepthShader.uniforms ),
			vertexShader: SSILVBDepthShader.vertexShader,
			fragmentShader: SSILVBDepthShader.fragmentShader,
			blending: NoBlending
		} );
		this.depthRenderMaterial.uniforms.cameraNear.value = this.camera.near;
		this.depthRenderMaterial.uniforms.cameraFar.value = this.camera.far;

		this.copyMaterial = new ShaderMaterial( {
			uniforms: UniformsUtils.clone( CopyShader.uniforms ),
			vertexShader: CopyShader.vertexShader,
			fragmentShader: CopyShader.fragmentShader,
			transparent: true,
			depthTest: false,
			depthWrite: false,
			blendSrc: DstColorFactor,
			blendDst: ZeroFactor,
			blendEquation: AddEquation,
			blendSrcAlpha: DstAlphaFactor,
			blendDstAlpha: ZeroFactor,
			blendEquationAlpha: AddEquation
		} );

		this.blendMaterial = new ShaderMaterial( {
			uniforms: UniformsUtils.clone( SSILVBBlendShader.uniforms ),
			vertexShader: SSILVBBlendShader.vertexShader,
			fragmentShader: SSILVBBlendShader.fragmentShader,
			transparent: true,
			depthTest: false,
			depthWrite: false,
			blending: CustomBlending,
			blendSrc: DstColorFactor,
			blendDst: ZeroFactor,
			blendEquation: AddEquation,
			blendSrcAlpha: DstAlphaFactor,
			blendDstAlpha: ZeroFactor,
			blendEquationAlpha: AddEquation
		} );

		this.fsQuad = new FullScreenQuad( null );

		this.originalClearColor = new Color();

		this.setGBuffer( parameters ? parameters.depthTexture : undefined, parameters ? parameters.normalTexture : undefined );

		if ( aoParameters !== undefined ) {

			this.updateSSILVBMaterial( aoParameters );

		}

		if ( pdParameters !== undefined ) {

			this.updatePdMaterial( pdParameters );

		}

	}

	dispose() {

		this.ssilvbNoiseTexture.dispose();
		this.pdNoiseTexture.dispose();
		this.normalRenderTarget.dispose();
		this.ssilvbRenderTarget.dispose();
		this.pdRenderTarget.dispose();
		this.normalMaterial.dispose();
		this.pdMaterial.dispose();
		this.copyMaterial.dispose();
		this.depthRenderMaterial.dispose();
		this.fsQuad.dispose();

	}

	get ssilvbMap() {

		return this.pdRenderTarget.texture;

	}

	setGBuffer( depthTexture, normalTexture, colorTexture ) {

		if ( depthTexture !== undefined ) {

			this.depthTexture = depthTexture;
			this.normalTexture = normalTexture;
			this.colorTexture = colorTexture;
			this._renderGBuffer = false;

		} else {

			this.depthTexture = new DepthTexture();
			this.depthTexture.format = DepthStencilFormat;
			this.depthTexture.type = UnsignedInt248Type;
			this.normalRenderTarget = new WebGLRenderTarget( this.width, this.height, {
				minFilter: NearestFilter,
				magFilter: NearestFilter,
				type: HalfFloatType,
				depthTexture: this.depthTexture
			} );
			this.normalTexture = this.normalRenderTarget.texture;
			//this.colorTexture = colorTexture;
			this._renderGBuffer = true;

		}

		const normalVectorType = ( this.normalTexture ) ? 1 : 0;
		const depthValueSource = ( this.depthTexture === this.normalTexture ) ? 'w' : 'x';

		this.ssilvbMaterial.defines.NORMAL_VECTOR_TYPE = normalVectorType;
		this.ssilvbMaterial.defines.DEPTH_SWIZZLING = depthValueSource;
		this.ssilvbMaterial.uniforms.tNormal.value = this.normalTexture;
		this.ssilvbMaterial.uniforms.tDepth.value = this.depthTexture;
		if(this.colorTexture) { this.ssilvbMaterial.uniforms.tColor.value = this.colorTexture;  }

		this.pdMaterial.defines.NORMAL_VECTOR_TYPE = normalVectorType;
		this.pdMaterial.defines.DEPTH_SWIZZLING = depthValueSource;
		this.pdMaterial.uniforms.tNormal.value = this.normalTexture;
		this.pdMaterial.uniforms.tDepth.value = this.depthTexture;

		this.depthRenderMaterial.uniforms.tDepth.value = this.normalRenderTarget.depthTexture;

	}

	setSceneClipBox( box ) {

		if ( box ) {

			this.ssilvbMaterial.needsUpdate = this.ssilvbMaterial.defines.SCENE_CLIP_BOX !== 1;
			this.ssilvbMaterial.defines.SCENE_CLIP_BOX = 1;
			this.ssilvbMaterial.uniforms.sceneBoxMin.value.copy( box.min );
			this.ssilvbMaterial.uniforms.sceneBoxMax.value.copy( box.max );

		} else {

			this.ssilvbMaterial.needsUpdate = this.ssilvbMaterial.defines.SCENE_CLIP_BOX === 0;
			this.ssilvbMaterial.defines.SCENE_CLIP_BOX = 0;

		}

	}

	updateSSILVBMaterial( parameters ) {

		if ( parameters.radius !== undefined ) {

			this.ssilvbMaterial.uniforms.radius.value = parameters.radius;

		}

		if ( parameters.distanceExponent !== undefined ) {

			this.ssilvbMaterial.uniforms.distanceExponent.value = parameters.distanceExponent;

		}

		if ( parameters.thickness !== undefined ) {

			this.ssilvbMaterial.uniforms.thickness.value = parameters.thickness;

		}

		if ( parameters.distanceFallOff !== undefined ) {

			this.ssilvbMaterial.uniforms.distanceFallOff.value = parameters.distanceFallOff;
			this.ssilvbMaterial.needsUpdate = true;

		}

		if ( parameters.scale !== undefined ) {

			this.ssilvbMaterial.uniforms.scale.value = parameters.scale;

		}

		if ( parameters.aosamples !== undefined && parameters.aosamples !== this.ssilvbMaterial.defines.SAMPLES ) {

			this.ssilvbMaterial.defines.SAMPLES = parameters.aosamples;
			this.ssilvbMaterial.needsUpdate = true;

		}

		if ( parameters.slices !== undefined && parameters.slices !== this.ssilvbMaterial.defines.SLICES ) {

			this.ssilvbMaterial.defines.SLICES = parameters.slices;
			this.ssilvbMaterial.needsUpdate = true;

		}

		if ( parameters.screenSpaceRadius !== undefined && ( parameters.screenSpaceRadius ? 1 : 0 ) !== this.ssilvbMaterial.defines.SCREEN_SPACE_RADIUS ) {

			this.ssilvbMaterial.defines.SCREEN_SPACE_RADIUS = parameters.screenSpaceRadius ? 1 : 0;
			this.ssilvbMaterial.needsUpdate = true;

		}

		if ( parameters.useCorrectNormals !== undefined ) {

			this.ssilvbMaterial.uniforms.useCorrectNormals.value = parameters.useCorrectNormals;

		}

	}

	updatePdMaterial( parameters ) {

		let updateShader = false;

		if ( parameters.lumaPhi !== undefined ) {

			this.pdMaterial.uniforms.lumaPhi.value = parameters.lumaPhi;

		}

		if ( parameters.depthPhi !== undefined ) {

			this.pdMaterial.uniforms.depthPhi.value = parameters.depthPhi;

		}

		if ( parameters.normalPhi !== undefined ) {

			this.pdMaterial.uniforms.normalPhi.value = parameters.normalPhi;

		}

		if ( parameters.radius !== undefined && parameters.radius !== this.radius ) {

			this.pdMaterial.uniforms.radius.value = parameters.radius;

		}

		if ( parameters.radiusExponent !== undefined && parameters.radiusExponent !== this.pdRadiusExponent ) {

			this.pdRadiusExponent = parameters.radiusExponent;
			updateShader = true;

		}

		if ( parameters.rings !== undefined && parameters.rings !== this.pdRings ) {

			this.pdRings = parameters.rings;
			updateShader = true;

		}

		if ( parameters.samples !== undefined && parameters.samples !== this.pdSamples ) {

			this.pdSamples = parameters.samples;
			updateShader = true;

		}

		if ( updateShader ) {

			this.pdMaterial.defines.SAMPLES = this.pdSamples;
			this.pdMaterial.defines.SAMPLE_VECTORS = generatePdSamplePointInitializer( this.pdSamples, this.pdRings, this.pdRadiusExponent );
			this.pdMaterial.needsUpdate = true;

		}

	}

	render( renderer, writeBuffer, readBuffer /*, deltaTime, maskActive */ ) {

		// render normals and depth (honor only meshes, points and lines do not contribute to AO)

		if ( this._renderGBuffer ) {

			this.overrideVisibility();
			this.renderOverride( renderer, this.normalMaterial, this.normalRenderTarget, 0x7777ff, 1.0 );
			this.restoreVisibility();

		}

		//this.setGBuffer( this.depthTexture, this.normalTexture, readBuffer );
		this.colorTexture = readBuffer.texture;
		if(this.colorTexture) { this.ssilvbMaterial.uniforms.tColor.value = this.colorTexture;  }
		this.ssilvbMaterial.needsUpdate = true;

		// render AO

		this.ssilvbMaterial.uniforms.cameraNear.value = this.camera.near;
		this.ssilvbMaterial.uniforms.cameraFar.value = this.camera.far;
		this.ssilvbMaterial.uniforms.cameraProjectionMatrix.value.copy( this.camera.projectionMatrix );
		this.ssilvbMaterial.uniforms.cameraProjectionMatrixInverse.value.copy( this.camera.projectionMatrixInverse );
		this.ssilvbMaterial.uniforms.cameraWorldMatrix.value.copy( this.camera.matrixWorld );
		this.renderPass( renderer, this.ssilvbMaterial, this.ssilvbRenderTarget, 0xffffff, 1.0 );

		// render poisson denoise

		this.pdMaterial.uniforms.cameraProjectionMatrixInverse.value.copy( this.camera.projectionMatrixInverse );
		this.renderPass( renderer, this.pdMaterial, this.pdRenderTarget, 0xffffff, 1.0 );

		// output result to screen

		switch ( this.output ) {

			case SSILVBPass.OUTPUT.Off:
				break;

			case SSILVBPass.OUTPUT.Diffuse:

				this.copyMaterial.uniforms.tDiffuse.value = readBuffer.texture;
				this.copyMaterial.blending = NoBlending;
				this.renderPass( renderer, this.copyMaterial, this.renderToScreen ? null : writeBuffer );

				break;

			case SSILVBPass.OUTPUT.AO:

				this.copyMaterial.uniforms.tDiffuse.value = this.ssilvbRenderTarget.texture;
				this.copyMaterial.blending = NoBlending;
				this.renderPass( renderer, this.copyMaterial, this.renderToScreen ? null : writeBuffer );

				break;

			case SSILVBPass.OUTPUT.Denoise:

				this.copyMaterial.uniforms.tDiffuse.value = this.pdRenderTarget.texture;
				this.copyMaterial.blending = NoBlending;
				this.renderPass( renderer, this.copyMaterial, this.renderToScreen ? null : writeBuffer );

				break;

			case SSILVBPass.OUTPUT.Depth:

				this.depthRenderMaterial.uniforms.cameraNear.value = this.camera.near;
				this.depthRenderMaterial.uniforms.cameraFar.value = this.camera.far;
				this.renderPass( renderer, this.depthRenderMaterial, this.renderToScreen ? null : writeBuffer );

				break;

			case SSILVBPass.OUTPUT.Normal:

				this.copyMaterial.uniforms.tDiffuse.value = this.normalRenderTarget.texture;
				this.copyMaterial.blending = NoBlending;
				this.renderPass( renderer, this.copyMaterial, this.renderToScreen ? null : writeBuffer );

				break;

			case SSILVBPass.OUTPUT.Default:

				this.copyMaterial.uniforms.tDiffuse.value = readBuffer.texture;
				this.copyMaterial.blending = NoBlending;
				this.renderPass( renderer, this.copyMaterial, this.renderToScreen ? null : writeBuffer );

				this.blendMaterial.uniforms.intensity.value = this.blendIntensity;
				this.blendMaterial.uniforms.tDiffuse.value = this.pdRenderTarget.texture;
				this.renderPass( renderer, this.blendMaterial, this.renderToScreen ? null : writeBuffer );

				break;

			default:
				console.warn( 'THREE.SSILVBPass: Unknown output type.' );

		}

	}

	renderPass( renderer, passMaterial, renderTarget, clearColor, clearAlpha ) {

		// save original state
		renderer.getClearColor( this.originalClearColor );
		const originalClearAlpha = renderer.getClearAlpha();
		const originalAutoClear = renderer.autoClear;

		renderer.setRenderTarget( renderTarget );

		// setup pass state
		renderer.autoClear = false;
		if ( ( clearColor !== undefined ) && ( clearColor !== null ) ) {

			renderer.setClearColor( clearColor );
			renderer.setClearAlpha( clearAlpha || 0.0 );
			renderer.clear();

		}

		this.fsQuad.material = passMaterial;
		this.fsQuad.render( renderer );

		// restore original state
		renderer.autoClear = originalAutoClear;
		renderer.setClearColor( this.originalClearColor );
		renderer.setClearAlpha( originalClearAlpha );

	}

	renderOverride( renderer, overrideMaterial, renderTarget, clearColor, clearAlpha ) {

		renderer.getClearColor( this.originalClearColor );
		const originalClearAlpha = renderer.getClearAlpha();
		const originalAutoClear = renderer.autoClear;

		renderer.setRenderTarget( renderTarget );
		renderer.autoClear = false;

		clearColor = overrideMaterial.clearColor || clearColor;
		clearAlpha = overrideMaterial.clearAlpha || clearAlpha;

		if ( ( clearColor !== undefined ) && ( clearColor !== null ) ) {

			renderer.setClearColor( clearColor );
			renderer.setClearAlpha( clearAlpha || 0.0 );
			renderer.clear();

		}

		this.scene.overrideMaterial = overrideMaterial;
		renderer.render( this.scene, this.camera );
		this.scene.overrideMaterial = null;

		renderer.autoClear = originalAutoClear;
		renderer.setClearColor( this.originalClearColor );
		renderer.setClearAlpha( originalClearAlpha );

	}

	setSize( width, height ) {

		this.width = width;
		this.height = height;

		this.ssilvbRenderTarget.setSize( width, height );
		this.normalRenderTarget.setSize( width, height );
		this.pdRenderTarget.setSize( width, height );

		this.ssilvbMaterial.uniforms.resolution.value.set( width, height );
		this.ssilvbMaterial.uniforms.cameraProjectionMatrix.value.copy( this.camera.projectionMatrix );
		this.ssilvbMaterial.uniforms.cameraProjectionMatrixInverse.value.copy( this.camera.projectionMatrixInverse );

		this.pdMaterial.uniforms.resolution.value.set( width, height );
		this.pdMaterial.uniforms.cameraProjectionMatrixInverse.value.copy( this.camera.projectionMatrixInverse );

	}

	overrideVisibility() {

		const scene = this.scene;
		const cache = this._visibilityCache;

		scene.traverse( function ( object ) {

			cache.set( object, object.visible );

			if ( object.isPoints || object.isLine ) object.visible = false;

		} );

	}

	restoreVisibility() {

		const scene = this.scene;
		const cache = this._visibilityCache;

		scene.traverse( function ( object ) {

			const visible = cache.get( object );
			object.visible = visible;

		} );

		cache.clear();

	}

	generateNoise( size = 64 ) {

		const simplex = new SimplexNoise();

		const arraySize = size * size * 4;
		const data = new Uint8Array( arraySize );

		for ( let i = 0; i < size; i ++ ) {

			for ( let j = 0; j < size; j ++ ) {

				const x = i;
				const y = j;

				data[ ( i * size + j ) * 4 ] = ( simplex.noise( x, y ) * 0.5 + 0.5 ) * 255;
				data[ ( i * size + j ) * 4 + 1 ] = ( simplex.noise( x + size, y ) * 0.5 + 0.5 ) * 255;
				data[ ( i * size + j ) * 4 + 2 ] = ( simplex.noise( x, y + size ) * 0.5 + 0.5 ) * 255;
				data[ ( i * size + j ) * 4 + 3 ] = ( simplex.noise( x + size, y + size ) * 0.5 + 0.5 ) * 255;

			}

		}

		const noiseTexture = new DataTexture( data, size, size, RGBAFormat, UnsignedByteType );
		noiseTexture.wrapS = RepeatWrapping;
		noiseTexture.wrapT = RepeatWrapping;
		noiseTexture.needsUpdate = true;

		return noiseTexture;

	}

}

SSILVBPass.OUTPUT = {
	'Off': - 1,
	'Default': 0,
	'Diffuse': 1,
	'Depth': 2,
	'Normal': 3,
	'AO': 4,
	'Denoise': 5,
};

export { SSILVBPass };
