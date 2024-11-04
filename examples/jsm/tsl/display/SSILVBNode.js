import { DataTexture, RenderTarget, RepeatWrapping, Vector2, Vector3, PostProcessingUtils } from 'three';
import { mod, round, select, length, sign, saturate, screenCoordinate, uint, depth, PI2, getScreenPosition, getViewPosition, QuadMesh, TempNode, nodeObject, Fn, float, NodeUpdateType, uv, uniform, Loop, vec2, vec3, vec4, int, dot, max, pow, abs, If, textureSize, sin, cos, PI, texture, passTexture, mat3, add, normalize, mul, cross, div, mix, sqrt, sub, acos, clamp, NodeMaterial } from 'three/tsl';

import { screenCoordinate, vec3, float, mul, vec4, cameraProjectionMatrixInverse, Fn, cameraWorldMatrix, normalize, cameraProjectionMatrix, vec2, int, mod, dot, sin, fract, abs, sub, sqrt, select, overloadingFn, uint, step, mix, min, clamp, color, uv, uvec2, Loop, uvec3, uvec4, ivec2, ivec3, ivec4, div, add, cos, length, If, acos, cross, max, bool, floor, pow, thickness, While, depth, saturate } from 'three/tsl';



const _quadMesh = /*@__PURE__*/ new QuadMesh();
const _size = /*@__PURE__*/ new Vector2();

let _rendererState;

class SSILVBNode extends TempNode {

	static get type() {

		return 'SSILVBNode';

	}

	constructor( colorNode, depthNode, normalNode, camera ) {

		super();

		this.colorNode = colorNode;
		this.depthNode = depthNode;
		this.normalNode = normalNode;

		this.radius = uniform( 0.25 );
		this.resolution = uniform( new Vector2() );
		this.thickness = uniform( 1 );
		this.distanceExponent = uniform( 1.7 );
		this.distanceFallOff = uniform( 1 );
		this.scale = uniform( 1 );
		this.noiseNode = texture( generateMagicSquareNoise() );

		this.cameraProjectionMatrix = uniform( camera.projectionMatrix );
		this.cameraProjectionMatrixInverse = uniform( camera.projectionMatrixInverse );

		this.SAMPLES = uniform( 16 );

		this._aoRenderTarget = new RenderTarget( 1, 1, { depthBuffer: false } );
		this._aoRenderTarget.texture.name = 'SSILVBNode.AO';

		this._material = null;
		this._textureNode = passTexture( this, this._aoRenderTarget.texture );

		this.updateBeforeType = NodeUpdateType.FRAME;

		this.useCorrectNormals = uniform( true );
		this.HALF_PI = float( mul( 0.5, PI ) );
		this.sliceCount = uniform( 4 );
		this.gl_FragCoord = vec3( screenCoordinate.x, screenCoordinate.y.oneMinus(), screenCoordinate.z ).toVar();
		this.pi = float( 3.14159265359 );
		this.halfPi = float( mul( 0.5, pi ) );
	}

	getTextureNode() {

		return this._textureNode;

	}

	setSize( width, height ) {

		this.resolution.value.set( width, height );
		this._aoRenderTarget.setSize( width, height );

	}

	updateBefore( frame ) {

		const { renderer } = frame;

		_rendererState = PostProcessingUtils.resetRendererState( renderer, _rendererState );

		//

		const size = renderer.getDrawingBufferSize( _size );
		this.setSize( size.width, size.height );

		_quadMesh.material = this._material;

		// clear

		renderer.setClearColor( 0xffffff, 1 );

		// ao

		renderer.setRenderTarget( this._aoRenderTarget );
		_quadMesh.render( renderer );

		// restore

		PostProcessingUtils.restoreRendererState( renderer, _rendererState );

	}

	setup( builder ) {

		const uvNode = uv();

		const sampleColor = ( uv ) => this.colorNode.uv( uv );
		const sampleDepth = ( uv ) => this.depthNode.uv( uv ).x;
		const sampleNoise = ( uv ) => this.noiseNode.uv( uv );
		const sampleNormal = ( uv ) => this.normalNode.uv( uv );
		

		export const getViewPosition = /*#__PURE__*/ Fn( ( [ screenPosition, depth ] ) => {

			const clipSpacePosition = vec4( vec3( screenPosition, depth ).mul( 2.0 ).sub( 1.0 ), 1.0 ).toVar();
			const viewSpacePosition = vec4( cameraProjectionMatrixInverse.mul( clipSpacePosition ) ).toVar();
		
			return viewSpacePosition.xyz.div( viewSpacePosition.w );
		
		} ).setLayout( {
			name: 'getViewPosition',
			type: 'vec3',
			inputs: [
				{ name: 'screenPosition', type: 'vec2', qualifier: 'in' },
				{ name: 'depth', type: 'float', qualifier: 'in' }
			]
		} );
		
		export const getWorldPosition = /*#__PURE__*/ Fn( ( [ screenPosition, depth ] ) => {
		
			const viewSpacePosition = vec3( getViewPosition( screenPosition, depth ) ).toVar();
		
			return cameraWorldMatrix.mul( vec4( viewSpacePosition, 1.0 ) )..( xyz );
		
		} ).setLayout( {
			name: 'getWorldPosition',
			type: 'vec3',
			inputs: [
				{ name: 'screenPosition', type: 'vec2', qualifier: 'in' },
				{ name: 'depth', type: 'float', qualifier: 'in' }
			]
		} );
		
		export const getDepth = /*#__PURE__*/ Fn( ( [ uv ] ) => {
		
			return textureLod( tDepth, uv.xy, 0.0 );
		
		} ).setLayout( {
			name: 'getDepth',
			type: 'float',
			inputs: [
				{ name: 'uv', type: 'vec2' }
			]
		} );
		
		export const getViewNormal = /*#__PURE__*/ Fn( ( [ uv ] ) => {
		
			return unpackRGBToNormal( textureLod( tNormal, uv, 0. ) );
		
		} ).setLayout( {
			name: 'getViewNormal',
			type: 'vec3',
			inputs: [
				{ name: 'uv', type: 'vec2' }
			]
		} );
		
		export const getWorldNormal = /*#__PURE__*/ Fn( ( [ uv ] ) => {
		
			return normalize( cameraWorldMatrix.mul( vec4( getViewNormal( uv ), 0.0 ) )..( xyz ) );
		
		} ).setLayout( {
			name: 'getWorldNormal',
			type: 'vec3',
			inputs: [
				{ name: 'uv', type: 'vec2' }
			]
		} );
		
		export const getSceneUvAndDepth = /*#__PURE__*/ Fn( ( [ sampleViewPos_immutable ] ) => {
		
			const sampleViewPos = vec3( sampleViewPos_immutable ).toVar();
			const sampleClipPos = vec4( cameraProjectionMatrix.mul( vec4( sampleViewPos, 1. ) ) ).toVar();
			const sampleUv = vec2( sampleClipPos.xy.div( sampleClipPos.w.mul( 0.5 ) ).add( 0.5 ) ).toVar();
			const sampleSceneDepth = float( getDepth( sampleUv ) ).toVar();
		
			return vec3( sampleUv, sampleSceneDepth );
		
		} ).setLayout( {
			name: 'getSceneUvAndDepth',
			type: 'vec3',
			inputs: [
				{ name: 'sampleViewPos', type: 'vec3' }
			]
		} );
		
		export const VPos_from_WPos = /*#__PURE__*/ Fn( ( [ wpos_immutable ] ) => {
		
			const wpos = vec3( wpos_immutable ).toVar();
			const vpos = vec3( cameraWorldMatrixInverse.mul( vec4( wpos, 1.0 ) )..( xyz ) ).toVar();
			vpos.z.mulAssign( - 1.0 );
		
			return vpos;
		
		} ).setLayout( {
			name: 'VPos_from_WPos',
			type: 'vec3',
			inputs: [
				{ name: 'wpos', type: 'vec3' }
			]
		} );
		
		export const VVec_from_WVec = /*#__PURE__*/ Fn( ( [ wvec_immutable ] ) => {
		
			const wvec = vec3( wvec_immutable ).toVar();
			const vvec = vec3( normalize( cameraWorldMatrixInverse.mul( vec4( wvec, 0.0 ) )..( xyz ) ) ).toVar();
			vvec.z.mulAssign( - 1.0 );
		
			return vvec;
		
		} ).setLayout( {
			name: 'VVec_from_WVec',
			type: 'vec3',
			inputs: [
				{ name: 'wvec', type: 'vec3' }
			]
		} );
		
		export const SPos_from_VPos = /*#__PURE__*/ Fn( ( [ vpos_immutable ] ) => {
		
			const vpos = vec3( vpos_immutable ).toVar();
			vpos.z.mulAssign( - 1.0 );
			const spos = vec3( getSceneUvAndDepth( vpos ) ).toVar();
		
			return vec3( spos.xy.mul( iResolution.xy ), spos.z );
		
		} ).setLayout( {
			name: 'SPos_from_VPos',
			type: 'vec3',
			inputs: [
				{ name: 'vpos', type: 'vec3' }
			]
		} );
		
		export const VPos_from_SPos = /*#__PURE__*/ Fn( ( [ spos_immutable ] ) => {
		
			const spos = vec3( spos_immutable ).toVar();
			const vpos = vec3( getViewPosition( spos.xy.div( iResolution.xy ), spos.z ) ).toVar();
			vpos.z.mulAssign( - 1.0 );
		
			return vpos;
		
		} ).setLayout( {
			name: 'VPos_from_SPos',
			type: 'vec3',
			inputs: [
				{ name: 'spos', type: 'vec3' }
			]
		} );
		
		export const randf = /*#__PURE__*/ Fn( ( [ x_immutable, y_immutable ] ) => {
		
			const y = int( y_immutable ).toVar();
			const x = int( x_immutable ).toVar();
		
			return mod( mul( 52.9829189, mod( mul( 0.06711056, float( x ) ).add( mul( 0.00583715, float( y ) ) ), 1.0 ) ), 1.0 );
		
		} ).setLayout( {
			name: 'randf',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'int' },
				{ name: 'y', type: 'int' }
			]
		} );
		
		export const rand2 = /*#__PURE__*/ Fn( ( [ co_immutable ] ) => {
		
			const co = vec2( co_immutable ).toVar();
			const a = float( 12.9898 ).toVar();
			const b = float( 78.233 ).toVar();
			const c = float( 43758.5453 ).toVar();
			const dt = float( dot( co.xy, vec2( a, b ) ) ).toVar();
			const sn = float( mod( dt, 3.14 ) ).toVar();
		
			return fract( sin( sn ).mul( c ) );
		
		} ).setLayout( {
			name: 'rand2',
			type: 'float',
			inputs: [
				{ name: 'co', type: 'vec2' }
			]
		} );
		
		export const GTAOFastAcos_0 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = vec2( x_immutable ).toVar();
			const outVal = vec2( - 0.156583.mul( abs( x ) ).add( halfPi ) ).toVar();
			outVal.mulAssign( sqrt( sub( 1.0, abs( x ) ) ) );
		
			return vec2( select( x.x.greaterThanEqual( 0.0 ), outVal.x, pi.sub( outVal.x ) ), select( x.y.greaterThanEqual( 0.0 ), outVal.y, pi.sub( outVal.y ) ) );
		
		} ).setLayout( {
			name: 'GTAOFastAcos_0',
			type: 'vec2',
			inputs: [
				{ name: 'x', type: 'vec2' }
			]
		} );
		
		export const GTAOFastAcos_1 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = float( x_immutable ).toVar();
			const outVal = float( - 0.156583.mul( abs( x ) ).add( halfPi ) ).toVar();
			outVal.mulAssign( sqrt( sub( 1.0, abs( x ) ) ) );
		
			return select( x.greaterThanEqual( 0.0 ), outVal, pi.sub( outVal ) );
		
		} ).setLayout( {
			name: 'GTAOFastAcos_1',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'float' }
			]
		} );
		
		export const GTAOFastAcos = /*#__PURE__*/ overloadingFn( [ GTAOFastAcos_0, GTAOFastAcos_1 ] );
		
		export const bitCount = /*#__PURE__*/ Fn( ( [ value_immutable ] ) => {
		
			const value = uint( value_immutable ).toVar();
			value.assign( value.sub( value.shiftRight( uint( 1 ) ).bitAnd( int( 0x55555555 ) ) ) );
			value.assign( value.bitAnd( int( 0x33333333 ) ).add( value.shiftRight( uint( 2 ) ).bitAnd( int( 0x33333333 ) ) ) );
		
			return value.add( value.shiftRight( uint( 4 ) ) ).bitAnd( int( 0xF0F0F0F ) ).mul( int( 0x1010101 ) ).shiftRight( uint( 24 ) );
		
		} ).setLayout( {
			name: 'bitCount',
			type: 'uint',
			inputs: [
				{ name: 'value', type: 'uint' }
			]
		} );
		
		export const RgbToHsv = /*#__PURE__*/ Fn( ( [ c_immutable ] ) => {
		
			const c = vec3( c_immutable ).toVar();
			const K = vec4( 0.0, - 1.0.div( 3.0 ), 2.0 / 3.0, - 1.0 ).toVar();
			const p = vec4( mix( vec4( c.bg, K.wz ), vec4( c.gb, K.xy ), step( c.b, c.g ) ) ).toVar();
			const q = vec4( mix( vec4( p.xyw, c.r ), vec4( c.r, p.yzx ), step( p.x, c.r ) ) ).toVar();
			const d = float( q.x.sub( min( q.w, q.y ) ) ).toVar();
			const e = float( 1.0e-10 ).toVar();
		
			return vec3( abs( q.z.add( q.w.sub( q.y ).div( mul( 6.0, d ).add( e ) ) ) ), d.div( q.x.add( e ) ), q.x );
		
		} ).setLayout( {
			name: 'RgbToHsv',
			type: 'vec3',
			inputs: [
				{ name: 'c', type: 'vec3' }
			]
		} );
		
		export const HsvToRgb = /*#__PURE__*/ Fn( ( [ c_immutable ] ) => {
		
			const c = vec3( c_immutable ).toVar();
			const K = vec4( 1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0 ).toVar();
			const p = vec3( abs( fract( c.xxx.add( K.xyz ) ).mul( 6.0 ).sub( K.www ) ) ).toVar();
		
			return c.z.mul( mix( K.xxx, clamp( p.sub( K.xxx ), 0.0, 1.0 ), c.y ) );
		
		} ).setLayout( {
			name: 'HsvToRgb',
			type: 'vec3',
			inputs: [
				{ name: 'c', type: 'vec3' }
			]
		} );
		
		export const Luminance = /*#__PURE__*/ Fn( ( [ color_immutable ] ) => {
		
			const color = vec3( color_immutable ).toVar();
		
			return dot( color, vec3( 0.299, 0.587, 0.114 ) );
		
		} ).setLayout( {
			name: 'Luminance',
			type: 'float',
			inputs: [
				{ name: 'color', type: 'vec3' }
			]
		} );
		
		export const IGN_0 = /*#__PURE__*/ Fn( ( [ uv_immutable ] ) => {
		
			const uv = vec2( uv_immutable ).toVar();
		
			return fract( mul( 52.9829189, fract( dot( uv, vec2( 0.06711056, 0.00583715 ) ) ) ) );
		
		} ).setLayout( {
			name: 'IGN_0',
			type: 'float',
			inputs: [
				{ name: 'uv', type: 'vec2' }
			]
		} );
		
		export const IGN_1 = /*#__PURE__*/ Fn( ( [ uv_immutable, frame_immutable ] ) => {
		
			const frame = uint( frame_immutable ).toVar();
			const uv = vec2( uv_immutable ).toVar();
			frame.assign( frame.remainder( uint( 64 ) ) );
			uv.addAssign( mul( 5.588238, float( frame ) ) );
		
			return IGN( uv );
		
		} ).setLayout( {
			name: 'IGN_1',
			type: 'float',
			inputs: [
				{ name: 'uv', type: 'vec2' },
				{ name: 'frame', type: 'uint' }
			]
		} );
		
		export const IGN = /*#__PURE__*/ overloadingFn( [ IGN_0, IGN_1 ] );
		
		export const EvalHilbertCurve = /*#__PURE__*/ Fn( ( [ uv_immutable, N_immutable ] ) => {
		
			const N = uint( N_immutable ).toVar();
			const uv = uvec2( uv_immutable ).toVar();
			const C = uint( int( 0xB4361E9C ) ).toVar();
			const P = uint( int( 0xEC7A9107 ) ).toVar();
			const c = uint( uint( 0 ) ).toVar();
			const p = uint( uint( 0 ) ).toVar();
		
			Loop( { start: N.sub( uint( 1 ) ), end: N, type: 'uint', update: '--' }, ( { i } ) => {
		
				const m = uvec2( uv.shiftRight( i ).bitAnd( uint( 1 ) ) ).toVar();
				const n = uint( m.x.bitXor( m.y.shiftLeft( uint( 1 ) ) ) ).toVar();
				const o = uint( p.shiftLeft( uint( 3 ) ).bitXor( n.shiftLeft( uint( 1 ) ) ) ).toVar();
				c.addAssign( C.shiftRight( o ).bitAnd( uint( 3 ) ).shiftLeft( i.shiftLeft( uint( 1 ) ) ) );
				p.assign( P.shiftRight( o ).bitAnd( uint( 3 ) ) );
		
			} );
		
			return c;
		
		} ).setLayout( {
			name: 'EvalHilbertCurve',
			type: 'uint',
			inputs: [
				{ name: 'uv', type: 'uvec2' },
				{ name: 'N', type: 'uint' }
			]
		} );
		
		export const reverse_bits = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uint( x_immutable ).toVar();
			x.assign( x.bitAnd( int( 0xaaaaaaaa ) ).shiftRight( int( 1 ) ).bitOr( x.bitAnd( int( 0x55555555 ) ).shiftLeft( int( 1 ) ) ) );
			x.assign( x.bitAnd( int( 0xcccccccc ) ).shiftRight( int( 2 ) ).bitOr( x.bitAnd( int( 0x33333333 ) ).shiftLeft( int( 2 ) ) ) );
			x.assign( x.bitAnd( int( 0xf0f0f0f0 ) ).shiftRight( int( 4 ) ).bitOr( x.bitAnd( int( 0x0f0f0f0f ) ).shiftLeft( int( 4 ) ) ) );
			x.assign( x.bitAnd( int( 0xff00ff00 ) ).shiftRight( int( 8 ) ).bitOr( x.bitAnd( int( 0x00ff00ff ) ).shiftLeft( int( 8 ) ) ) );
		
			return x.shiftRight( int( 16 ) ).bitOr( x.shiftLeft( int( 16 ) ) );
		
		} ).setLayout( {
			name: 'reverse_bits',
			type: 'uint',
			inputs: [
				{ name: 'x', type: 'uint' }
			]
		} );
		
		export const laine_karras_permutation = /*#__PURE__*/ Fn( ( [ x_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const x = uint( x_immutable ).toVar();
			x.addAssign( seed );
			x.bitXorAssign( x.mul( int( 0x6c50b47c ) ) );
			x.bitXorAssign( x.mul( int( 0xb82f1e52 ) ) );
			x.bitXorAssign( x.mul( int( 0xc7afe638 ) ) );
			x.bitXorAssign( x.mul( int( 0x8d22f6e6 ) ) );
		
			return x;
		
		} ).setLayout( {
			name: 'laine_karras_permutation',
			type: 'uint',
			inputs: [
				{ name: 'x', type: 'uint' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const nested_uniform_scramble = /*#__PURE__*/ Fn( ( [ x_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const x = uint( x_immutable ).toVar();
			x.assign( reverse_bits( x ) );
			x.assign( laine_karras_permutation( x, seed ) );
			x.assign( reverse_bits( x ) );
		
			return x;
		
		} ).setLayout( {
			name: 'nested_uniform_scramble',
			type: 'uint',
			inputs: [
				{ name: 'x', type: 'uint' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const shuffled_scrambled_sobol_angle01 = /*#__PURE__*/ Fn( ( [ x_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const x = uint( x_immutable ).toVar();
			x.assign( reverse_bits( x ) );
			x.assign( laine_karras_permutation( x, seed ) );
		
			return x;
		
		} ).setLayout( {
			name: 'shuffled_scrambled_sobol_angle01',
			type: 'uint',
			inputs: [
				{ name: 'x', type: 'uint' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const asuint2_0 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = float( x_immutable ).toVar();
		
			return select( x.equal( 0.0 ), uint( 0 ), floatBitsToUint( x ) );
		
		} ).setLayout( {
			name: 'asuint2_0',
			type: 'uint',
			inputs: [
				{ name: 'x', type: 'float' }
			]
		} );
		
		export const asuint2_1 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = vec2( x_immutable ).toVar();
		
			return uvec2( asuint2( x.x ), asuint2( x.y ) );
		
		} ).setLayout( {
			name: 'asuint2_1',
			type: 'uvec2',
			inputs: [
				{ name: 'x', type: 'vec2' }
			]
		} );
		
		export const asuint2_2 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = vec3( x_immutable ).toVar();
		
			return uvec3( asuint2( x.xy ), asuint2( x.z ) );
		
		} ).setLayout( {
			name: 'asuint2_2',
			type: 'uvec3',
			inputs: [
				{ name: 'x', type: 'vec3' }
			]
		} );
		
		export const asuint2_3 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = vec4( x_immutable ).toVar();
		
			return uvec4( asuint2( x.xy ), asuint2( x.zw ) );
		
		} ).setLayout( {
			name: 'asuint2_3',
			type: 'uvec4',
			inputs: [
				{ name: 'x', type: 'vec4' }
			]
		} );
		
		export const asuint2 = /*#__PURE__*/ overloadingFn( [ asuint2_0, asuint2_1, asuint2_2, asuint2_3 ] );
		
		export const Float01_0 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uint( x_immutable ).toVar();
		
			return float( x ).mul( 1.0 / 4294967296.0 );
		
		} ).setLayout( {
			name: 'Float01_0',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'uint' }
			]
		} );
		
		export const Float11_0 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uint( x_immutable ).toVar();
		
			return float( int( x ) ).mul( 1.0 / 2147483648.0 );
		
		} ).setLayout( {
			name: 'Float11_0',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'uint' }
			]
		} );
		
		export const Float01_1 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uvec2( x_immutable ).toVar();
		
			return vec2( x ).mul( 1.0 / 4294967296.0 );
		
		} ).setLayout( {
			name: 'Float01_1',
			type: 'vec2',
			inputs: [
				{ name: 'x', type: 'uvec2' }
			]
		} );
		
		export const Float11_1 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uvec2( x_immutable ).toVar();
		
			return vec2( ivec2( x ) ).mul( 1.0 / 2147483648.0 );
		
		} ).setLayout( {
			name: 'Float11_1',
			type: 'vec2',
			inputs: [
				{ name: 'x', type: 'uvec2' }
			]
		} );
		
		export const Float01_2 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uvec3( x_immutable ).toVar();
		
			return vec3( x ).mul( 1.0 / 4294967296.0 );
		
		} ).setLayout( {
			name: 'Float01_2',
			type: 'vec3',
			inputs: [
				{ name: 'x', type: 'uvec3' }
			]
		} );
		
		export const Float11_2 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uvec3( x_immutable ).toVar();
		
			return vec3( ivec3( x ) ).mul( 1.0 / 2147483648.0 );
		
		} ).setLayout( {
			name: 'Float11_2',
			type: 'vec3',
			inputs: [
				{ name: 'x', type: 'uvec3' }
			]
		} );
		
		export const Float01_3 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uvec4( x_immutable ).toVar();
		
			return vec4( x ).mul( 1.0 / 4294967296.0 );
		
		} ).setLayout( {
			name: 'Float01_3',
			type: 'vec4',
			inputs: [
				{ name: 'x', type: 'uvec4' }
			]
		} );
		
		export const Float01 = /*#__PURE__*/ overloadingFn( [ Float01_0, Float01_1, Float01_2, Float01_3 ] );
		
		export const Float11_3 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uvec4( x_immutable ).toVar();
		
			return vec4( ivec4( x ) ).mul( 1.0 / 2147483648.0 );
		
		} ).setLayout( {
			name: 'Float11_3',
			type: 'vec4',
			inputs: [
				{ name: 'x', type: 'uvec4' }
			]
		} );
		
		export const Float11 = /*#__PURE__*/ overloadingFn( [ Float11_0, Float11_1, Float11_2, Float11_3 ] );
		
		const Pi = float( 3.14159265359 );
		const RcpPi = float( div( 1.0, Pi ) );
		const Pi05 = float( Pi.mul( 0.5 ) );
		const RcpPi05 = float( div( 1.0, Pi05 ) );
		
		export const Pow2 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = float( x_immutable ).toVar();
		
			return x.mul( x );
		
		} ).setLayout( {
			name: 'Pow2',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'float' }
			]
		} );
		
		export const Pow3 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = float( x_immutable ).toVar();
		
			return x.mul( x ).mul( x );
		
		} ).setLayout( {
			name: 'Pow3',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'float' }
			]
		} );
		
		export const Pow4 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = float( x_immutable ).toVar();
		
			return Pow2( Pow2( x ) );
		
		} ).setLayout( {
			name: 'Pow4',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'float' }
			]
		} );
		
		const rPhif1 = float( 0.6180340 );
		const rPhif2 = vec2( 0.7548777, 0.5698403 );
		const rPhif3 = vec3( 0.8191725, 0.6710436, 0.5497005 );
		const rPhif4 = vec4( 0.8566749, 0.7338919, 0.6287067, 0.5385973 );
		const rPhi1 = uint( uint( 2654435769 ) );
		const rPhi2 = uvec2( uint( 3242174889 ), uint( 2447445413 ) );
		const rPhi3 = uvec3( uint( 3518319153 ), uint( 2882110345 ), uint( 2360945575 ) );
		const rPhi4 = uvec4( uint( 3679390609 ), uint( 3152041523 ), uint( 2700274805 ), uint( 2313257605 ) );
		
		export const WellonsHash = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uint( x_immutable ).toVar();
			x.bitXorAssign( x.shiftRight( uint( 16 ) ) );
			x.mulAssign( int( 0x7feb352dU ) );
			x.bitXorAssign( x.shiftRight( uint( 15 ) ) );
			x.mulAssign( int( 0x846ca68bU ) );
			x.bitXorAssign( x.shiftRight( uint( 16 ) ) );
		
			return x;
		
		} ).setLayout( {
			name: 'WellonsHash',
			type: 'uint',
			inputs: [
				{ name: 'x', type: 'uint' }
			]
		} );
		
		export const WellonsHash2 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = uint( x_immutable ).toVar();
			x.bitXorAssign( x.shiftRight( uint( 17 ) ) );
			x.mulAssign( int( 0xed5ad4bbU ) );
			x.bitXorAssign( x.shiftRight( uint( 11 ) ) );
			x.mulAssign( int( 0xac4c1b51U ) );
			x.bitXorAssign( x.shiftRight( uint( 15 ) ) );
			x.mulAssign( int( 0x31848babU ) );
			x.bitXorAssign( x.shiftRight( uint( 14 ) ) );
		
			return x;
		
		} ).setLayout( {
			name: 'WellonsHash2',
			type: 'uint',
			inputs: [
				{ name: 'x', type: 'uint' }
			]
		} );
		
		export const WeylHash = /*#__PURE__*/ Fn( ( [ c_immutable ] ) => {
		
			const c = uvec2( c_immutable ).toVar();
		
			return c.x.mul( int( 0x3504f333 ) ).bitXor( c.y.mul( int( 0xf1bbcdcb ) ) ).mul( uint( 741103597 ) );
		
		} ).setLayout( {
			name: 'WeylHash',
			type: 'uint',
			inputs: [
				{ name: 'c', type: 'uvec2' }
			]
		} );
		
		const lcgM = uint( uint( 2891336453 ) );
		
		export const lcg = /*#__PURE__*/ Fn( ( [ h_immutable ] ) => {
		
			const h = uint( h_immutable ).toVar();
		
			return h.mul( lcgM ).add( int( 0x5C995C6D ) );
		
		} ).setLayout( {
			name: 'lcg',
			type: 'uint',
			inputs: [
				{ name: 'h', type: 'uint' }
			]
		} );
		
		export const pcg3Mix = /*#__PURE__*/ Fn( ( [ h_immutable ] ) => {
		
			const h = uvec3( h_immutable ).toVar();
			h.x.addAssign( h.y.mul( h.z ) );
			h.y.addAssign( h.z.mul( h.x ) );
			h.z.addAssign( h.x.mul( h.y ) );
		
			return h;
		
		} ).setLayout( {
			name: 'pcg3Mix',
			type: 'uvec3',
			inputs: [
				{ name: 'h', type: 'uvec3' }
			]
		} );
		
		export const pcg3Permute = /*#__PURE__*/ Fn( ( [ h_immutable ] ) => {
		
			const h = uvec3( h_immutable ).toVar();
			h.assign( pcg3Mix( h ) );
			h.bitXorAssign( h.shiftRight( uint( 16 ) ) );
		
			return pcg3Mix( h );
		
		} ).setLayout( {
			name: 'pcg3Permute',
			type: 'uvec3',
			inputs: [
				{ name: 'h', type: 'uvec3' }
			]
		} );
		
		export const pcg3_0 = /*#__PURE__*/ Fn( ( [ state ] ) => {
		
			state.assign( lcg( state ) );
		
			return pcg3Permute( uvec3( uint( 2447445413 ), state, uint( 3242174889 ) ) );
		
		} );
		
		export const pcg3_1 = /*#__PURE__*/ Fn( ( [ h_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const h = uvec3( h_immutable ).toVar();
			const c = uvec3( seed.shiftLeft( uint( 1 ) ).bitXor( uvec3( int( 0x5C995C6D ), int( 0x6A3C6A57 ), int( 0xC65536CB ) ) ) ).toVar();
		
			return pcg3Permute( h.mul( lcgM ).add( c ) );
		
		} ).setLayout( {
			name: 'pcg3_1',
			type: 'uvec3',
			inputs: [
				{ name: 'h', type: 'uvec3' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const pcg3 = /*#__PURE__*/ overloadingFn( [ pcg3_0, pcg3_1 ] );
		
		export const pcg4Mix = /*#__PURE__*/ Fn( ( [ h_immutable ] ) => {
		
			const h = uvec4( h_immutable ).toVar();
			h.x.addAssign( h.y.mul( h.w ) );
			h.y.addAssign( h.z.mul( h.x ) );
			h.z.addAssign( h.x.mul( h.y ) );
			h.w.addAssign( h.y.mul( h.z ) );
		
			return h;
		
		} ).setLayout( {
			name: 'pcg4Mix',
			type: 'uvec4',
			inputs: [
				{ name: 'h', type: 'uvec4' }
			]
		} );
		
		export const pcg4Permute = /*#__PURE__*/ Fn( ( [ h_immutable ] ) => {
		
			const h = uvec4( h_immutable ).toVar();
			h.assign( pcg4Mix( h ) );
			h.bitXorAssign( h.shiftRight( uint( 16 ) ) );
		
			return pcg4Mix( h );
		
		} ).setLayout( {
			name: 'pcg4Permute',
			type: 'uvec4',
			inputs: [
				{ name: 'h', type: 'uvec4' }
			]
		} );
		
		export const pcg4_0 = /*#__PURE__*/ Fn( ( [ state ] ) => {
		
			state.assign( lcg( state ) );
		
			return pcg4Permute( uvec4( uint( 2882110345 ), state, uint( 3518319153 ), uint( 2360945575 ) ) );
		
		} );
		
		export const pcg4_1 = /*#__PURE__*/ Fn( ( [ h_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const h = uvec4( h_immutable ).toVar();
			const c = uvec4( seed.shiftLeft( uint( 1 ) ).bitXor( uvec4( int( 0x5C995C6D ), int( 0x6A3C6A57 ), int( 0xC65536CB ), int( 0x3563995F ) ) ) ).toVar();
		
			return pcg4Permute( h.mul( lcgM ).add( c ) );
		
		} ).setLayout( {
			name: 'pcg4_1',
			type: 'uvec4',
			inputs: [
				{ name: 'h', type: 'uvec4' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const pcg4 = /*#__PURE__*/ overloadingFn( [ pcg4_0, pcg4_1 ] );
		
		export const pcg_0 = /*#__PURE__*/ Fn( ( [ state ] ) => {
		
			state.assign( lcg( state ) );
			const word = uint( state.shiftRight( state.shiftRight( uint( 28 ) ).add( uint( 4 ) ) ).bitXor( state ).mul( uint( 277803737 ) ) ).toVar();
		
			return word.shiftRight( uint( 22 ) ).bitXor( word );
		
		} );
		
		export const pcg_1 = /*#__PURE__*/ Fn( ( [ h_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const h = uint( h_immutable ).toVar();
			const c = uint( seed.shiftLeft( uint( 1 ) ).bitXor( int( 0x5C995C6D ) ) ).toVar();
			h.assign( h.mul( lcgM ).add( c ) );
			h.assign( h.shiftRight( h.shiftRight( uint( 28 ) ).add( uint( 4 ) ) ).bitXor( h ).mul( uint( 277803737 ) ) );
		
			return h.shiftRight( uint( 22 ) ).bitXor( h );
		
		} ).setLayout( {
			name: 'pcg_1',
			type: 'uint',
			inputs: [
				{ name: 'h', type: 'uint' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const pcg = /*#__PURE__*/ overloadingFn( [ pcg_0, pcg_1 ] );
		
		export const ACos_Approx = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = float( x_immutable ).toVar();
			const u = float( add( 1.5708, - 0.204912.add( mul( 0.0483293, abs( x ) ) ).mul( abs( x ) ) ) ).toVar();
			u.mulAssign( sqrt( sub( 1.0, abs( x ) ) ) );
		
			return select( x.greaterThanEqual( 0.0 ), u, Pi.sub( u ) );
		
		} ).setLayout( {
			name: 'ACos_Approx',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'float' }
			]
		} );
		
		export const Hash = /*#__PURE__*/ Fn( ( [ h_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const h = uvec2( h_immutable ).toVar();
		
			return pcg3( uvec3( h, uint( 0 ) ), seed );
		
		} ).setLayout( {
			name: 'Hash',
			type: 'uvec2',
			inputs: [
				{ name: 'h', type: 'uvec2' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const Hash01x3_0 = /*#__PURE__*/ Fn( ( [ v_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const v = uvec3( v_immutable ).toVar();
		
			return Float01( pcg3( v, seed ) );
		
		} ).setLayout( {
			name: 'Hash01x3_0',
			type: 'vec3',
			inputs: [
				{ name: 'v', type: 'uvec3' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const Hash01x3_1 = /*#__PURE__*/ Fn( ( [ v_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const v = uint( v_immutable ).toVar();
		
			return Hash01x3( uvec3( v, uint( 0 ), uint( 0 ) ), seed );
		
		} ).setLayout( {
			name: 'Hash01x3_1',
			type: 'vec3',
			inputs: [
				{ name: 'v', type: 'uint' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const Hash01x3 = /*#__PURE__*/ overloadingFn( [ Hash01x3_0, Hash01x3_1 ] );
		
		export const Hash01x2 = /*#__PURE__*/ Fn( ( [ v_immutable, seed_immutable ] ) => {
		
			const seed = uint( seed_immutable ).toVar();
			const v = uint( v_immutable ).toVar();
		
			return Hash01x3( uvec3( v, uint( 0 ), uint( 0 ) ), seed );
		
		} ).setLayout( {
			name: 'Hash01x2',
			type: 'vec2',
			inputs: [
				{ name: 'v', type: 'uint' },
				{ name: 'seed', type: 'uint' }
			]
		} );
		
		export const ACos_0 = /*#__PURE__*/ Fn( ( [ x_immutable ] ) => {
		
			const x = float( x_immutable ).toVar();
		
			return ACos_Approx( x );
		
		} ).setLayout( {
			name: 'ACos_0',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'float' }
			]
		} );
		
		export const ACos_1 = /*#__PURE__*/ Fn( ( [ v_immutable ] ) => {
		
			const v = vec2( v_immutable ).toVar();
		
			return vec2( ACos( v.x ), ACos( v.y ) );
		
		} ).setLayout( {
			name: 'ACos_1',
			type: 'vec2',
			inputs: [
				{ name: 'v', type: 'vec2' }
			]
		} );
		
		export const ACos = /*#__PURE__*/ overloadingFn( [ ACos_0, ACos_1 ] );
		
		export const SampleSliceDir = /*#__PURE__*/ Fn( ( [ vvsN_immutable, rnd01_immutable ] ) => {
		
			const rnd01 = float( rnd01_immutable ).toVar();
			const vvsN = vec3( vvsN_immutable ).toVar();
			const ang0 = float( rnd01.mul( Pi ) ).toVar();
			const dir0 = vec2( cos( ang0 ), sin( ang0 ) ).toVar();
			const l = float( length( vvsN.xy ) ).toVar();
		
			If( l.equal( 0.0 ), () => {
		
				return dir0;
		
			} );
		
			dir0.mulAssign( select( dot( dir0, vvsN.xy ).lessThan( 0.0 ), - 1.0, 1.0 ) );
			const n = vec2( vvsN.xy.div( l ) ).toVar();
			const dir = vec2().toVar();
			const x = float( dir0.x.mul( n.y ).sub( dir0.y.mul( n.x ) ) ).toVar();
			const s = float( l ).toVar();
			s.addAssign( s.sub( s.mul( s ) ).mul( 0.15 ) );
			const y = float( acos( x.mul( sin( s.mul( Pi05 ) ) ) ).mul( RcpPi05 ) ).toVar();
			const ys = float( div( 1.0, s ) ).toVar();
			dir.y.assign( ys.sub( ys.mul( y ) ) );
			dir.x.assign( sqrt( clamp( sub( 1.0, dir.y.mul( dir.y ) ), 0.0, 1.0 ) ) );
		
			return vec2( dir.x.mul( n.x ).sub( dir.y.mul( n.y ) ), dir.y.mul( n.x ).add( dir.x.mul( n.y ) ) );
		
		} ).setLayout( {
			name: 'SampleSliceDir',
			type: 'vec2',
			inputs: [
				{ name: 'vvsN', type: 'vec3' },
				{ name: 'rnd01', type: 'float' }
			]
		} );
		
		export const GetQuaternion_0 = /*#__PURE__*/ Fn( ( [ from_immutable, to_immutable ] ) => {
		
			const to = vec3( to_immutable ).toVar();
			const from = vec3( from_immutable ).toVar();
			const xyz = vec3( cross( from, to ) ).toVar();
			const s = float( dot( from, to ) ).toVar();
			const u = float( inversesqrt( max( 0.0, s.mul( 0.5 ).add( 0.5 ) ) ) ).toVar();
			s.assign( div( 1.0, u ) );
			xyz.mulAssign( u.mul( 0.5 ) );
		
			return vec4( xyz, s );
		
		} ).setLayout( {
			name: 'GetQuaternion_0',
			type: 'vec4',
			inputs: [
				{ name: 'from', type: 'vec3' },
				{ name: 'to', type: 'vec3' }
			]
		} );
		
		export const GetQuaternion_1 = /*#__PURE__*/ Fn( ( [ to_immutable ] ) => {
		
			const to = vec3( to_immutable ).toVar();
			const xyz = vec3( to.y.negate(), to.x, 0.0 ).toVar();
			const s = float( to.z ).toVar();
			const u = float( inversesqrt( max( 0.0, s.mul( 0.5 ).add( 0.5 ) ) ) ).toVar();
			s.assign( div( 1.0, u ) );
			xyz.mulAssign( u.mul( 0.5 ) );
		
			return vec4( xyz, s );
		
		} ).setLayout( {
			name: 'GetQuaternion_1',
			type: 'vec4',
			inputs: [
				{ name: 'to', type: 'vec3' }
			]
		} );
		
		export const GetQuaternion = /*#__PURE__*/ overloadingFn( [ GetQuaternion_0, GetQuaternion_1 ] );
		
		export const Transform = /*#__PURE__*/ Fn( ( [ v_immutable, q_immutable ] ) => {
		
			const q = vec4( q_immutable ).toVar();
			const v = vec3( v_immutable ).toVar();
			const k = vec3( cross( q.xyz, v ) ).toVar();
		
			return v.add( mul( 2.0, vec3( dot( vec3( q.wy, q.z.negate() ), k.xzy ), dot( vec3( q.wz, q.x.negate() ), k.yxz ), dot( vec3( q.wx, q.y.negate() ), k.zyx ) ) ) );
		
		} ).setLayout( {
			name: 'Transform',
			type: 'vec3',
			inputs: [
				{ name: 'v', type: 'vec3' },
				{ name: 'q', type: 'vec4' }
			]
		} );
		
		export const Transform_Qz0 = /*#__PURE__*/ Fn( ( [ v_immutable, q_immutable ] ) => {
		
			const q = vec4( q_immutable ).toVar();
			const v = vec3( v_immutable ).toVar();
			const k = float( v.y.mul( q.x ).sub( v.x.mul( q.y ) ) ).toVar();
			const g = float( mul( 2.0, v.z.mul( q.w ).add( k ) ) ).toVar();
			const r = vec3().toVar();
			r.xy.assign( v.xy.add( q.yx.mul( vec2( g, g.negate() ) ) ) );
			r.z.assign( v.z.add( mul( 2.0, q.w.mul( k ).sub( v.z.mul( dot( q.xy, q.xy ) ) ) ) ) );
		
			return r;
		
		} ).setLayout( {
			name: 'Transform_Qz0',
			type: 'vec3',
			inputs: [
				{ name: 'v', type: 'vec3' },
				{ name: 'q', type: 'vec4' }
			]
		} );
		
		export const Transform_Vz0Qz0 = /*#__PURE__*/ Fn( ( [ v_immutable, q_immutable ] ) => {
		
			const q = vec4( q_immutable ).toVar();
			const v = vec2( v_immutable ).toVar();
			const o = float( q.x.mul( v.y ) ).toVar();
			const c = float( q.y.mul( v.x ) ).toVar();
			const b = vec3( o.sub( c ), o.negate().add( c ), o.sub( c ) ).toVar();
		
			return vec3( v, 0.0 ).add( mul( 2.0, b.mul( q.yxw ) ) );
		
		} ).setLayout( {
			name: 'Transform_Vz0Qz0',
			type: 'vec3',
			inputs: [
				{ name: 'v', type: 'vec2' },
				{ name: 'q', type: 'vec4' }
			]
		} );
		
		export const CountBits = /*#__PURE__*/ Fn( ( [ v_immutable ] ) => {
		
			const v = uint( v_immutable ).toVar();
			v.assign( v.sub( v.shiftRight( uint( 1 ) ).bitAnd( int( 0x55555555 ) ) ) );
			v.assign( v.bitAnd( int( 0x33333333 ) ).add( v.shiftRight( uint( 2 ) ).bitAnd( int( 0x33333333 ) ) ) );
		
			return v.add( v.shiftRight( uint( 4 ) ) ).bitAnd( int( 0xF0F0F0F ) ).mul( int( 0x1010101 ) ).shiftRight( uint( 24 ) );
		
		} ).setLayout( {
			name: 'CountBits',
			type: 'uint',
			inputs: [
				{ name: 'v', type: 'uint' }
			]
		} );
		
		export const SliceRelCDF_Uniform = /*#__PURE__*/ Fn( ( [ x_immutable, angN_immutable ] ) => {
		
			const angN = float( angN_immutable ).toVar();
			const x = float( x_immutable ).toVar();
			const phi = float( x.mul( Pi ).sub( Pi05 ) ).toVar();
			const c = bool( phi.greaterThanEqual( angN ) ).toVar();
			const m0 = float( select( c, 2.0, 0.0 ) ).toVar();
			const m1 = float( select( c, - 1.0, 1.0 ) ).toVar();
			const d0 = float( mul( 0.5, m0.add( m1.mul( cos( angN.sub( phi ) ) ) ).add( sin( angN ) ) ) ).toVar();
		
			return d0;
		
		} ).setLayout( {
			name: 'SliceRelCDF_Uniform',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'float' },
				{ name: 'angN', type: 'float' }
			]
		} );
		
		export const SliceRelCDF_Cos = /*#__PURE__*/ Fn( ( [ x_immutable, angN_immutable ] ) => {
		
			const angN = float( angN_immutable ).toVar();
			const x = float( x_immutable ).toVar();
			const phi = float( x.mul( Pi ).sub( Pi05 ) ).toVar();
			const c = bool( phi.greaterThanEqual( angN ) ).toVar();
			const n0 = float( select( c, 3.0, 1.0 ) ).toVar();
			const n1 = float( select( c, - 1.0, 1.0 ) ).toVar();
			const n2 = float( select( c, 4.0, 0.0 ) ).toVar();
			const t0 = float( n0.mul( cos( angN ) ).add( n1.mul( cos( angN.sub( mul( 2.0, phi ) ) ) ) ).add( n2.mul( angN ).add( n1.mul( 2.0 ).mul( phi ) ).add( Pi ).mul( sin( angN ) ) ) ).toVar();
			const t1 = float( mul( 4.0, cos( angN ).add( angN.mul( sin( angN ) ) ) ) ).toVar();
		
			return t0.div( t1 );
		
		} ).setLayout( {
			name: 'SliceRelCDF_Cos',
			type: 'float',
			inputs: [
				{ name: 'x', type: 'float' },
				{ name: 'angN', type: 'float' }
			]
		} );
		
		export const GTVBAO = /*#__PURE__*/ Fn( ( [ uv0_immutable, wpos_immutable, N_immutable, pxId_immutable, dirCount_immutable ] ) => {
		
			const dirCount = uint( dirCount_immutable ).toVar();
			const pxId = uint( pxId_immutable ).toVar();
			const N = vec3( N_immutable ).toVar();
			const wpos = vec3( wpos_immutable ).toVar();
			const uv0 = vec2( uv0_immutable ).toVar();
			const positionVS = vec3( VPos_from_WPos( wpos ) ).toVar();
			const normalVS = vec3( VVec_from_WVec( N ) ).toVar();
			const isPerspectiveCam = bool( true ).toVar();
			const V = vec3( select( isPerspectiveCam, normalize( positionVS ).negate(), vec3( 0.0, 0.0, - 1.0 ) ) ).toVar();
			const rayStart = vec2( SPos_from_VPos( positionVS ) ).toVar();
			const ao = float( 0.0 ).toVar();
		
			Loop( { start: uint( 0 ), end: dirCount, type: 'uint' }, ( { i } ) => {
		
				const h = uint( pxId.mul( dirCount ).add( i ) ).toVar();
				const smplDirVS = vec3().toVar();
				const dir = vec2().toVar();
				const rnd01 = float( IGN( floor( uv0 ), frameNumber ) ).toVar();
				const Q_toV = vec4( GetQuaternion( V ) ).toVar();
				const Q_fromV = vec4( Q_toV.mul( vec4( vec3( - 1.0 ), 1.0 ) ) ).toVar();
				const normalVVS = vec3( normalVS ).toVar();
		
				If( isPerspectiveCam, () => {
		
					normalVVS.assign( Transform_Qz0( normalVS, Q_fromV ) );
		
				} );
		
				dir.assign( SampleSliceDir( normalVVS, rnd01 ) );
				smplDirVS.assign( vec3( dir.xy, 0.0 ) );
		
				If( isPerspectiveCam, () => {
		
					smplDirVS.assign( Transform_Vz0Qz0( dir, Q_toV ) );
					const rayStart = vec3( SPos_from_VPos( positionVS ) ).toVar();
					const rayEnd = vec3( SPos_from_VPos( positionVS.add( smplDirVS.mul( 1.0 * 0.5 ) ) ) ).toVar();
					const rayDir = vec3( rayEnd.sub( rayStart ) ).toVar();
					rayDir.divAssign( length( rayDir.xy ) );
					dir.assign( rayDir.xy );
		
				} );
		
				const cosN = float().toVar(), angN = float().toVar(), projNRcpLen = float().toVar();
				const sliceN = vec3( cross( V, smplDirVS ) ).toVar();
				const projN = vec3( normalVS.sub( sliceN.mul( dot( normalVS, sliceN ) ) ) ).toVar();
				const projNSqrLen = float( dot( projN, projN ) ).toVar();
		
				If( projNSqrLen.equal( 0.0 ), () => {
		
					return vec3( 1.0 );
		
				} );
		
				projNRcpLen.assign( inversesqrt( projNSqrLen ) );
				cosN.assign( dot( projN, V ).mul( projNRcpLen ) );
				const T = vec3( cross( sliceN, projN ) ).toVar();
				const sgn = float( select( dot( V, T ).lessThan( 0.0 ), - 1.0, 1.0 ) ).toVar();
				angN.assign( sgn.mul( ACos( cosN ) ) );
				const rnd01 = vec2( Hash01x2( h, int( 0x968CC604 ) ) ).toVar();
				const occBits = uint( uint( 0 ) ).toVar();
		
				{
		
					const d = float( - 1.0 ).toVar();
		
					While( d.lessThanEqual( 1.0 ), () => {
		
						const rayDir = vec2( dir.xy.mul( d ) ).toVar();
						const count = float( SAMPLES );
						const s = float( pow( radius, div( 1.0, count ) ) ).toVar();
						const t = float( pow( s, rnd01.x ) ).toVar();
						rnd01.x.assign( sub( 1.0, rnd01.x ) );
		
						Loop( { start: 0.0, end: count, type: 'float' }, ( { i } ) => {
		
							const samplePos = vec2( rayStart.add( rayDir.mul( t ) ) ).toVar();
							t.mulAssign( s );
		
							If( samplePos.x.lessThan( 0.0 ).or( samplePos.x.greaterThanEqual( iResolution.x ) ).or( samplePos.y.lessThan( 0.0 ) ).or( samplePos.y.greaterThanEqual( iResolution.y ) ), () => {
		
								break;
		
							} );
		
							const sampleDepth = float( getDepth( samplePos.div( iResolution.xy ) ) ).toVar();
							const samplePosVS = vec3( VPos_from_SPos( vec3( samplePos, sampleDepth ) ) ).toVar();
							const Thickness = float( thickness ).toVar();
							const deltaPosFront = vec3( samplePosVS.sub( positionVS ) ).toVar();
							const deltaPosBack = vec3( deltaPosFront.sub( V.mul( Thickness ) ) ).toVar();
		
							If( isPerspectiveCam, () => {
		
								deltaPosBack.assign( deltaPosFront.add( normalize( samplePosVS ).mul( Thickness ) ) );
		
							} );
		
							const horCos = vec2( dot( normalize( deltaPosFront ), V ), dot( normalize( deltaPosBack ), V ) ).toVar();
							const horAng = vec2( ACos( horCos ).mul( d ) ).toVar();
							const hor01 = vec2( clamp( horAng.add( angN ).mul( RcpPi ).add( 0.5 ), 0.0, 1.0 ) ).toVar();
							hor01.assign( select( d.greaterThanEqual( 0.0 ), hor01.xy, hor01.yx ) );
							hor01.x.assign( SliceRelCDF_Cos( hor01.x, angN ) );
							hor01.y.assign( SliceRelCDF_Cos( hor01.y, angN ) );
							hor01.assign( clamp( hor01.add( rnd01.y.mul( 1.0 / 32.0 ) ), 0.0, 1.0 ) );
							const occBits0 = uint().toVar();
							const horInt = uvec2( floor( hor01.mul( 32.0 ) ) ).toVar();
							const OxFFFFFFFFu = uint( int( 0xFFFFFFFF ) ).toVar();
							const mX = uint( select( horInt.x.lessThan( uint( 32 ) ), OxFFFFFFFFu.shiftLeft( horInt.x ), uint( 0 ) ) ).toVar();
							const mY = uint( select( horInt.y.!=( uint( 0 ) ), OxFFFFFFFFu.shiftRight( uint( 32 ).sub( horInt.y ) ), uint( 0 ) ) ).toVar();
							occBits0.assign( mX.bitAnd( mY ) );
							occBits.assign( occBits.bitOr( occBits0 ) );
		
						} );
		
						d.addAssign( 2.0 );
		
					} )
		
				}
		
				const occ0 = float( float( CountBits( occBits ) ).mul( 1.0 / 32.0 ) ).toVar();
				const slice_weight = float( 1.0 ).toVar();
				ao.addAssign( sub( 1.0, occ0 ).mul( slice_weight ) );
		
			} );
		
			ao.divAssign( float( dirCount ) );
		
			return vec3( ao );
		
		} ).setLayout( {
			name: 'GTVBAO',
			type: 'vec3',
			inputs: [
				{ name: 'uv0', type: 'vec2' },
				{ name: 'wpos', type: 'vec3' },
				{ name: 'N', type: 'vec3' },
				{ name: 'pxId', type: 'uint' },
				{ name: 'dirCount', type: 'uint' }
			]
		} );
		
		export const main = /*#__PURE__*/ Fn( () => {
		
			const depth = float( getDepth( vUv.xy ) ).toVar();
		
			If( depth.greaterThanEqual( 1.0 ), () => {
		
				discard;
		
				return;
		
			} );
		
			const uv0 = vec2( gl_FragCoord.xy ).toVar();
			const uvu = uvec2( uv0.xy.sub( 0.5 ) ).toVar();
			uvu.addAssign( Hash( uvec2( uint( frameNumber ), uint( 0 ) ), int( 0xBD1E0BB0 ) ) );
			const wpos = vec3( getWorldPosition( vUv, depth ) ).toVar();
			const N = vec3( normalize( getWorldNormal( vUv.xy ) ) ).toVar();
			wpos.addAssign( N.mul( 1.0 / 1024.0 ) );
			const pxId = uint( EvalHilbertCurve( uvu, uint( 9 ) ) ).toVar();
			const count = uint( SLICES ).toVar();
			const ssao = vec3( GTVBAO( uv0, wpos, N, pxId, count ) ).toVar();
			gl_FragColor.assign( vec4( vec3( saturate( pow( saturate( ssao.x ), scale ) ) ), 1.0 ) );
		
		} ).setLayout( {
			name: 'main',
			type: 'void',
			inputs: []
		} );
		
		const sectorCount = uint( uint( 32 ) );

		const ao = Fn( () => {
		
			const indirect = uint( uint( 0 ) ).toVar();
			const occlusion = uint( uint( 0 ) ).toVar();
			const visibility = float( 0.0 ).toVar();
			const lighting = vec3( 0.0 ).toVar();
			const frontBackHorizon = vec2( 0.0 ).toVar();
			const aspect = vec2( this.cameraProjectionMatrix.element( int( 0 ) ).element( int( 0 ) ).div( this.cameraProjectionMatrix.element( int( 1 ) ).element( int( 1 ) ) ), 1.0 ).toVar();
			
			const depth = sampleDepth( uvNode ).toVar();
			depth.greaterThanEqual( 1.0 ).discard();
			
			const position = getViewPosition( uvNode, depth, this.cameraProjectionMatrixInverse ).toVar();
			position.z.negateAssign();
			const normal = this.normalNode.rgb.normalize().toVar();
			normal.z.negateAssign();
			const camera = vec3( normalize( position.negate() ) ).toVar();
		
			const sliceRotation = float( PI2.div( float( this.sliceCount ) ) ).toVar();
			const sampleScale = float( this.radius.negate().mul( this.cameraProjectionMatrix.element( int( 0 ) ).element( int( 0 ) ) ).div( position.z ) ).toVar();
			const sampleOffset = float( mul( 0.01, this.radius ) ).toVar();
			const jitter = float( randf( int( this.gl_FragCoord.x ), int( this.gl_FragCoord.y ) ).sub( 0.5 ) ).toVar();

			Loop( { end: this.sliceCount, type: 'int', name: 'slice', condition: '<' }, ( { slice } ) => {
		
				const phi = sliceRotation.mul( float( slice ).add( jitter ) ).toVar();
				const omega = vec2( cos( phi ), sin( phi ) ).toVar();
				const direction = vec3( omega.x, omega.y, 0.0 ).toVar();
				const orthoDirection = vec3( direction.sub( dot( direction, camera ).mul( camera ) ) ).toVar();
				const axis = vec3( cross( direction, camera ) ).toVar();
				const projNormal = vec3( normal.sub( axis.mul( dot( normal, axis ) ) ) ).toVar();
				const projLength = float( length( projNormal ) ).toVar();
				const signN = float( sign( dot( orthoDirection, projNormal ) ) ).toVar();
				const cosN = float( clamp( dot( projNormal, camera ).div( projLength ), 0.0, 1.0 ) ).toVar();
				const n = float( signN.mul( acos( cosN ) ) ).toVar();
		

				Loop( { start: 0.0, end: this.SAMPLES.add( 0.5 ), type: 'float', name: 'currentSample', condition: '<' }, ( { currentSample } ) => {
		
					const sampleStep = float( currentSample.add( jitter.mul( 5.0 ) ).div( this.SAMPLES ).add( sampleOffset ) ).toVar();
					const sampleUV = vec2( uvNode.sub( sampleStep.mul( sampleScale ).mul( omega ).mul( aspect ) ) ).toVar();
					const samplePosition = vec3( getViewPosition( sampleUV, sampleDepth( sampleUV ), this.cameraProjectionMatrixInverse ) ).toVar();
					samplePosition.z.negateAssign();
					
					const sampleNormalL = vec3( normalize( sampleNormal( sampleUV ) ) ).toVar();
					const sampleLight = vec3( sampleColor( sampleUV ) ).toVar();
					const sampleDistance = vec3( samplePosition.sub( position ) ).toVar();
					const sampleLength = float( length( sampleDistance ) ).toVar();
					const sampleHorizon = vec3( sampleDistance.div( sampleLength ) ).toVar();
					frontBackHorizon.x.assign( dot( sampleHorizon, camera ) );
					frontBackHorizon.y.assign( dot( normalize( sampleDistance.sub( camera.mul( this.thickness ) ) ), camera ) );
					frontBackHorizon.assign( acos( frontBackHorizon ) );
					frontBackHorizon.assign( clamp( frontBackHorizon.add( n ).add( this.HALF_PI ).div( PI ), 0.0, 1.0 ) );
					indirect.assign( updateSectors( frontBackHorizon.x, frontBackHorizon.y, uint( 0 ) ) );
					lighting.addAssign( sub( 1.0, float( bitCount( indirect.bitAnd( occlusion.bitNot() ) ) ).div( float( sectorCount ) ) ).mul( sampleLight ).mul( clamp( dot( normal, sampleHorizon ), 0.0, 1.0 ) ).mul( clamp( dot( sampleNormalL, sampleHorizon.negate() ), 0.0, 1.0 ) ) );
					occlusion.bitOrAssign( indirect );
		
				} )
		
				visibility.addAssign( sub( 1.0, float( bitCount( occlusion ) ).div( float( sectorCount ) ) ) );
		
			} )
		
		
			visibility.divAssign( this.sliceCount );
			lighting.divAssign( this.sliceCount );
			visibility.assign( saturate( pow( saturate( visibility ), this.scale ) ) );
			return vec4( visibility, visibility, visibility, 1.0 );
		
		} );

		const material = this._material || ( this._material = new NodeMaterial() );
		material.fragmentNode = ao().context( builder.getSharedContext() );
		material.name = 'SSILVB';
		material.needsUpdate = true;

		//

		return this._textureNode;

	}

	dispose() {

		this._aoRenderTarget.dispose();

	}

}

export default SSILVBNode;

function generateMagicSquareNoise( size = 5 ) {

	const noiseSize = Math.floor( size ) % 2 === 0 ? Math.floor( size ) + 1 : Math.floor( size );
	const magicSquare = generateMagicSquare( noiseSize );
	const noiseSquareSize = magicSquare.length;
	const data = new Uint8Array( noiseSquareSize * 4 );

	for ( let inx = 0; inx < noiseSquareSize; ++ inx ) {

		const iAng = magicSquare[ inx ];
		const angle = ( 2 * Math.PI * iAng ) / noiseSquareSize;
		const randomVec = new Vector3(
			Math.cos( angle ),
			Math.sin( angle ),
			0
		).normalize();
		data[ inx * 4 ] = ( randomVec.x * 0.5 + 0.5 ) * 255;
		data[ inx * 4 + 1 ] = ( randomVec.y * 0.5 + 0.5 ) * 255;
		data[ inx * 4 + 2 ] = 127;
		data[ inx * 4 + 3 ] = 255;

	}

	const noiseTexture = new DataTexture( data, noiseSize, noiseSize );
	noiseTexture.wrapS = RepeatWrapping;
	noiseTexture.wrapT = RepeatWrapping;
	noiseTexture.needsUpdate = true;

	return noiseTexture;

}

function generateMagicSquare( size ) {

	const noiseSize = Math.floor( size ) % 2 === 0 ? Math.floor( size ) + 1 : Math.floor( size );
	const noiseSquareSize = noiseSize * noiseSize;
	const magicSquare = Array( noiseSquareSize ).fill( 0 );
	let i = Math.floor( noiseSize / 2 );
	let j = noiseSize - 1;

	for ( let num = 1; num <= noiseSquareSize; ) {

		if ( i === - 1 && j === noiseSize ) {

			j = noiseSize - 2;
			i = 0;

		} else {

			if ( j === noiseSize ) {

				j = 0;

			}

			if ( i < 0 ) {

				i = noiseSize - 1;

			}

		}

		if ( magicSquare[ i * noiseSize + j ] !== 0 ) {

			j -= 2;
			i ++;
			continue;

		} else {

			magicSquare[ i * noiseSize + j ] = num ++;

		}

		j ++;
		i --;

	}

	return magicSquare;

}

export const ao = ( colorNode, depthNode, normalNode, camera ) => nodeObject( new SSILVBNode( nodeObject( colorNode ), nodeObject( depthNode ), nodeObject( normalNode ), camera ) );
