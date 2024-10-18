import { DataTexture, RenderTarget, RepeatWrapping, Vector2, Vector3, PostProcessingUtils } from 'three';
import { mod, round, select, length, sign, saturate, screenCoordinate, uint, depth, PI2, getScreenPosition, getViewPosition, QuadMesh, TempNode, nodeObject, Fn, float, NodeUpdateType, uv, uniform, Loop, vec2, vec3, vec4, int, dot, max, pow, abs, If, textureSize, sin, cos, PI, texture, passTexture, mat3, add, normalize, mul, cross, div, mix, sqrt, sub, acos, clamp, NodeMaterial } from 'three/tsl';

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
		this.sliceCount = float( 8.0 );
		this.gl_FragCoord = vec3( screenCoordinate.x, screenCoordinate.y.oneMinus(), screenCoordinate.z ).toVar();

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
		
		const randf = /*#__PURE__*/ Fn( ( [ x_immutable, y_immutable ] ) => {
		
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
		
		const bitCount = /*#__PURE__*/ Fn( ( [ value_immutable ] ) => {
		
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
		
		const sectorCount = uint( uint( 32 ) );
		
		const updateSectors = /*#__PURE__*/ Fn( ( [ minHorizon_immutable, maxHorizon_immutable, outBitfield_immutable ] ) => {
		
			const outBitfield = uint( outBitfield_immutable ).toVar();
			const maxHorizon = float( maxHorizon_immutable ).toVar();
			const minHorizon = float( minHorizon_immutable ).toVar();
			const startBit = uint( minHorizon.mul( float( sectorCount ) ) ).toVar();
			const horizonAngle = uint( round( maxHorizon.sub( minHorizon ).mul( float( sectorCount ) ) ) ).toVar();
			const angleBit = uint( select( horizonAngle.greaterThan( uint( 0 ) ), uint( int( 0xFFFFFFFF ).shiftRight( sectorCount.sub( horizonAngle ) ) ), uint( 0 ) ) ).toVar();
			const currentBitfield = uint( angleBit.shiftLeft( startBit ) ).toVar();
		
			return outBitfield.bitOr( currentBitfield );
		
		} ).setLayout( {
			name: 'updateSectors',
			type: 'uint',
			inputs: [
				{ name: 'minHorizon', type: 'float' },
				{ name: 'maxHorizon', type: 'float' },
				{ name: 'outBitfield', type: 'uint' }
			]
		} );
		
		const ao = Fn( () => {
		
			const indirect = uint( uint( 0 ) ).toVar();
			const occlusion = uint( uint( 0 ) ).toVar();
			const visibility = float( 0.0 ).toVar();
			const lighting = vec3( 0.0 ).toVar();
			const frontBackHorizon = vec2( 0.0 ).toVar();
			const aspect = vec2( this.cameraProjectionMatrix.element( int( 0 ) ).element( int( 0 ) ).div( this.cameraProjectionMatrix.element( int( 1 ) ).element( int( 1 ) ) ), 1.0 ).toVar();
			depth.greaterThanEqual( 1.0 ).discard();

			
			const position = getViewPosition( uvNode, depth, this.cameraProjectionMatrixInverse ).toVar();
			const normal = this.normalNode.rgb.normalize().toVar();
			const camera = vec3( normalize( position.negate() ) ).toVar();
		
			//If( this.useCorrectNormals.not(), () => {
			//	normal.xyz.assign( normal.xyz.mul( 0.5 ).add( 0.5 ) );
			//} );
		
			const sliceRotation = float( PI2.div( float( this.sliceCount.sub( 1.0 ) ) ) ).toVar();
			const sampleScale = float( this.radius.negate().mul( this.cameraProjectionMatrix.element( int( 0 ) ).element( int( 0 ) ) ).div( position.z ) ).toVar();
			const sampleOffset = float( mul( 0.01, this.radius ) ).toVar();
			const jitter = float( randf( int( this.gl_FragCoord.x ), int( this.gl_FragCoord.y ) ).sub( 0.5 ) ).toVar();
		
			{
		
				//const slice = float( 0.0 ).toVar();
		
				//While( slice.lessThan( this.sliceCount.add( 0.5 ) ), () => {
				Loop( { start: 0.0, end: this.sliceCount.add( 0.5 ), type: 'float', name: 'slice', condition: '<' }, ( { slice } ) => {
		
					const phi = float( sliceRotation.mul( slice.add( jitter ) ).add( PI ) ).toVar();
					const omega = vec2( cos( phi ), sin( phi ) ).toVar();
					const direction = vec3( omega.x, omega.y, 0.0 ).toVar();
					const orthoDirection = vec3( direction.sub( dot( direction, camera ).mul( camera ) ) ).toVar();
					const axis = vec3( cross( direction, camera ) ).toVar();
					const projNormal = vec3( normal.sub( axis.mul( dot( normal, axis ) ) ) ).toVar();
					const projLength = float( length( projNormal ) ).toVar();
					const signN = float( sign( dot( orthoDirection, projNormal ) ) ).toVar();
					const cosN = float( clamp( dot( projNormal, camera ).div( projLength ), 0.0, 1.0 ) ).toVar();
					const n = float( signN.mul( acos( cosN ) ) ).toVar();
		
					{
		
						//const currentSample = float( 0.0 ).toVar();
		
						//While( currentSample.lessThan( this.SAMPLES.add( 0.5 ) ), () => {
						Loop( { start: 0.0, end: this.SAMPLES.add( 0.5 ), type: 'float', name: 'currentSample', condition: '<' }, ( { currentSample } ) => {
		
							const sampleStep = float( currentSample.add( jitter.mul( 5.0 ) ).div( this.SAMPLES ).add( sampleOffset ) ).toVar();
							const sampleUV = vec2( uvNode.sub( sampleStep.mul( sampleScale ).mul( omega ).mul( aspect ) ) ).toVar();
							const samplePosition = vec3( getViewPosition( sampleUV, sampleDepth( sampleUV ), this.cameraProjectionMatrixInverse ) ).toVar();
							
							
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
		
							currentSample.addAssign( 1.0 );
		
						} )
		
					}
		
					visibility.addAssign( sub( 1.0, float( bitCount( occlusion ) ).div( float( sectorCount ) ) ) );
		
					slice.addAssign( 1.0 );
		
				} )
		
			}
		
			visibility.divAssign( this.sliceCount );
			lighting.divAssign( this.sliceCount );
			visibility.assign( saturate( pow( saturate( visibility ), this.scale ) ) );
			return vec4( visibility, visibility, visibility, 1.0 );
		
		} );
		


		/*
		const ao = Fn( () => {

			const depth = sampleDepth( uvNode ).toVar();

			depth.greaterThanEqual( 1.0 ).discard();

			const viewPosition = getViewPosition( uvNode, depth, this.cameraProjectionMatrixInverse ).toVar();
			const viewNormal = this.normalNode.rgb.normalize().toVar();

			const radiusToUse = this.radius;

			const noiseResolution = textureSize( this.noiseNode, 0 );
			let noiseUv = vec2( uvNode.x, uvNode.y.oneMinus() );
			noiseUv = noiseUv.mul( this.resolution.div( noiseResolution ) );
			const noiseTexel = sampleNoise( noiseUv );
			const randomVec = noiseTexel.xyz.mul( 2.0 ).sub( 1.0 );
			const tangent = vec3( randomVec.xy, 0.0 ).normalize();
			const bitangent = vec3( tangent.y.mul( - 1.0 ), tangent.x, 0.0 );
			const kernelMatrix = mat3( tangent, bitangent, vec3( 0.0, 0.0, 1.0 ) );

			const DIRECTIONS = this.SAMPLES.lessThan( 30 ).select( 3, 5 ).toVar();
			const STEPS = add( this.SAMPLES, DIRECTIONS.sub( 1 ) ).div( DIRECTIONS ).toVar();

			const ao = float( 0 ).toVar();

			Loop( { start: int( 0 ), end: DIRECTIONS, type: 'int', condition: '<' }, ( { i } ) => {

				const angle = float( i ).div( float( DIRECTIONS ) ).mul( PI ).toVar();
				const sampleDir = vec4( cos( angle ), sin( angle ), 0., add( 0.5, mul( 0.5, noiseTexel.w ) ) );
				sampleDir.xyz = normalize( kernelMatrix.mul( sampleDir.xyz ) );

				const viewDir = normalize( viewPosition.xyz.negate() ).toVar();
				const sliceBitangent = normalize( cross( sampleDir.xyz, viewDir ) ).toVar();
				const sliceTangent = cross( sliceBitangent, viewDir );
				const normalInSlice = normalize( viewNormal.sub( sliceBitangent.mul( dot( viewNormal, sliceBitangent ) ) ) );

				const tangentToNormalInSlice = cross( normalInSlice, sliceBitangent ).toVar();
				const cosHorizons = vec2( dot( viewDir, tangentToNormalInSlice ), dot( viewDir, tangentToNormalInSlice.negate() ) ).toVar();

				Loop( { end: STEPS, type: 'int', name: 'j', condition: '<' }, ( { j } ) => {

					const sampleViewOffset = sampleDir.xyz.mul( radiusToUse ).mul( sampleDir.w ).mul( pow( div( float( j ).add( 1.0 ), float( STEPS ) ), this.distanceExponent ) );

					// x

					const sampleScreenPositionX = getScreenPosition( viewPosition.add( sampleViewOffset ), this.cameraProjectionMatrix ).toVar();
					const sampleDepthX = sampleDepth( sampleScreenPositionX ).toVar();
					const sampleSceneViewPositionX = getViewPosition( sampleScreenPositionX, sampleDepthX, this.cameraProjectionMatrixInverse ).toVar();
					const viewDeltaX = sampleSceneViewPositionX.sub( viewPosition ).toVar();

					If( abs( viewDeltaX.z ).lessThan( this.thickness ), () => {

						const sampleCosHorizon = dot( viewDir, normalize( viewDeltaX ) );
						cosHorizons.x.addAssign( max( 0, mul( sampleCosHorizon.sub( cosHorizons.x ), mix( 1.0, float( 2.0 ).div( float( j ).add( 2 ) ), this.distanceFallOff ) ) ) );

					} );

					// y

					const sampleScreenPositionY = getScreenPosition( viewPosition.sub( sampleViewOffset ), this.cameraProjectionMatrix ).toVar();
					const sampleDepthY = sampleDepth( sampleScreenPositionY ).toVar();
					const sampleSceneViewPositionY = getViewPosition( sampleScreenPositionY, sampleDepthY, this.cameraProjectionMatrixInverse ).toVar();
					const viewDeltaY = sampleSceneViewPositionY.sub( viewPosition ).toVar();

					If( abs( viewDeltaY.z ).lessThan( this.thickness ), () => {

						const sampleCosHorizon = dot( viewDir, normalize( viewDeltaY ) );
						cosHorizons.y.addAssign( max( 0, mul( sampleCosHorizon.sub( cosHorizons.y ), mix( 1.0, float( 2.0 ).div( float( j ).add( 2 ) ), this.distanceFallOff ) ) ) );

					} );

				} );

				const sinHorizons = sqrt( sub( 1.0, cosHorizons.mul( cosHorizons ) ) ).toVar();
				const nx = dot( normalInSlice, sliceTangent );
				const ny = dot( normalInSlice, viewDir );
				const nxb = mul( 0.5, acos( cosHorizons.y ).sub( acos( cosHorizons.x ) ).add( sinHorizons.x.mul( cosHorizons.x ).sub( sinHorizons.y.mul( cosHorizons.y ) ) ) );
				const nyb = mul( 0.5, sub( 2.0, cosHorizons.x.mul( cosHorizons.x ) ).sub( cosHorizons.y.mul( cosHorizons.y ) ) );
				const occlusion = nx.mul( nxb ).add( ny.mul( nyb ) );
				ao.addAssign( occlusion );

			} );

			ao.assign( clamp( ao.div( DIRECTIONS ), 0, 1 ) );
			ao.assign( pow( ao, this.scale ) );

			return vec4( vec3( ao ), 1.0 );

		} );*/

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
