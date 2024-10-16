import {
	DataTexture,
	Matrix4,
	RepeatWrapping,
	Vector2,
	Vector3,
} from 'three';

/**
 * References:
 * - implemented algorithm - SSILVB
 *   - https://cybereality.com/screen-space-indirect-lighting-with-visibility-bitmask-improvement-to-gtao-ssao-real-time-ambient-occlusion-algorithm-glsl-shader-implementation/
 *   - https://cdrinmatane.github.io/posts/ssaovb-code/
 */

const SSILVBShader = {

	name: 'SSILVBShader',

	defines: {
		PERSPECTIVE_CAMERA: 1,
		SAMPLES: 16,
		NORMAL_VECTOR_TYPE: 1,
		DEPTH_SWIZZLING: 'x',
		SCREEN_SPACE_RADIUS: 0.0,
		SCREEN_SPACE_RADIUS_SCALE: 100.0,
		SCENE_CLIP_BOX: 0.0,
	},

	uniforms: {
		tNormal: { value: null },
		tDepth: { value: null },
		tColor: { value: null },
		tNoise: { value: null },
		resolution: { value: new Vector2() },
		cameraNear: { value: null },
		cameraFar: { value: null },
		cameraProjectionMatrix: { value: new Matrix4() },
		cameraProjectionMatrixInverse: { value: new Matrix4() },
		cameraWorldMatrix: { value: new Matrix4() },
		radius: { value: 0.1 },
		distanceExponent: { value: 1. },
		thickness: { value: 1. },
		distanceFallOff: { value: 1. },
		scale: { value: 1. },
		sceneBoxMin: { value: new Vector3( - 1, - 1, - 1 ) },
		sceneBoxMax: { value: new Vector3( 1, 1, 1 ) },
		directionSwizzle: { value: false },
	},

	vertexShader: /* glsl */`

		varying vec2 vUv;

		void main() {
			vUv = uv;
			gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
		}`,

	fragmentShader: /* glsl */`
		// Adapted from "Screen Space Indirect Lighting with Visibility Bitmask" by Olivier Therrien, et al.
		// https://cdrinmatane.github.io/posts/cgspotlight-slides/

		precision highp float;
		precision highp sampler2D;

		varying vec2 vUv;
		uniform sampler2D tNormal;
		uniform sampler2D tDepth;
		uniform sampler2D tColor;
		uniform sampler2D tNoise;

		uniform vec2 resolution;
		uniform float cameraNear;
		uniform float cameraFar;
		uniform mat4 cameraProjectionMatrix;
		uniform mat4 cameraProjectionMatrixInverse;		
		uniform mat4 cameraWorldMatrix;
		uniform float radius;
		uniform float distanceExponent;
		uniform float thickness;
		uniform float scale;
		uniform bool directionSwizzle;
		#if SCENE_CLIP_BOX == 1
			uniform vec3 sceneBoxMin;
			uniform vec3 sceneBoxMax;
		#endif

		const float pi = 3.14159265359;
		const float twoPi = 2.0 * pi;
		const float halfPi = 0.5 * pi;
		const float sliceCount = 4.0;
		float sampleCount = float(SAMPLES);

		#include <common>
		#include <packing>

		vec3 getViewPosition(const in vec2 screenPosition, const in float depth) {
			vec4 clipSpacePosition = vec4(vec3(screenPosition, depth) * 2.0 - 1.0, 1.0);
			vec4 viewSpacePosition = cameraProjectionMatrixInverse * clipSpacePosition;
			return viewSpacePosition.xyz / viewSpacePosition.w;
		}

		vec3 getWorldPosition(const in vec2 screenPosition, const in float depth) {
			vec3 viewSpacePosition = getViewPosition(screenPosition, depth);
			return (cameraWorldMatrix * vec4(viewSpacePosition, 1.0)).xyz;
		}

		float getDepth(const vec2 uv) {  
			return textureLod(tDepth, uv.xy, 0.0).DEPTH_SWIZZLING;
		}

		float fetchDepth(const ivec2 uv) {   
			return texelFetch(tDepth, uv.xy, 0).DEPTH_SWIZZLING;
		}

		float getViewZ(const in float depth) {
			#if PERSPECTIVE_CAMERA == 1
				return perspectiveDepthToViewZ(depth, cameraNear, cameraFar);
			#else
				return orthographicDepthToViewZ(depth, cameraNear, cameraFar);
			#endif
		}

		vec3 computeNormalFromDepth(const vec2 uv) {
			vec2 size = vec2(textureSize(tDepth, 0));
			ivec2 p = ivec2(uv * size);
			float c0 = fetchDepth(p);
			float l2 = fetchDepth(p - ivec2(2, 0));
			float l1 = fetchDepth(p - ivec2(1, 0));
			float r1 = fetchDepth(p + ivec2(1, 0));
			float r2 = fetchDepth(p + ivec2(2, 0));
			float b2 = fetchDepth(p - ivec2(0, 2));
			float b1 = fetchDepth(p - ivec2(0, 1));
			float t1 = fetchDepth(p + ivec2(0, 1));
			float t2 = fetchDepth(p + ivec2(0, 2));
			float dl = abs((2.0 * l1 - l2) - c0);
			float dr = abs((2.0 * r1 - r2) - c0);
			float db = abs((2.0 * b1 - b2) - c0);
			float dt = abs((2.0 * t1 - t2) - c0);
			vec3 ce = getViewPosition(uv, c0).xyz;
			vec3 dpdx = (dl < dr) ? ce - getViewPosition((uv - vec2(1.0 / size.x, 0.0)), l1).xyz : -ce + getViewPosition((uv + vec2(1.0 / size.x, 0.0)), r1).xyz;
			vec3 dpdy = (db < dt) ? ce - getViewPosition((uv - vec2(0.0, 1.0 / size.y)), b1).xyz : -ce + getViewPosition((uv + vec2(0.0, 1.0 / size.y)), t1).xyz;
			return normalize(cross(dpdx, dpdy));
		}

		vec3 getViewNormal(const vec2 uv) {
			#if NORMAL_VECTOR_TYPE == 2
				return normalize(textureLod(tNormal, uv, 0.).rgb);
			#elif NORMAL_VECTOR_TYPE == 1
				return unpackRGBToNormal(textureLod(tNormal, uv, 0.).rgb);
			#else
				return computeNormalFromDepth(uv);
			#endif
		}

		vec3 getWorldNormal(const vec2 uv) {
			return normalize((cameraWorldMatrix * vec4(getViewNormal(uv), 0.0)).xyz);
		}

		vec3 getSceneUvAndDepth(vec3 sampleViewPos) {
			vec4 sampleClipPos = cameraProjectionMatrix * vec4(sampleViewPos, 1.);
			vec2 sampleUv = sampleClipPos.xy / sampleClipPos.w * 0.5 + 0.5;
			float sampleSceneDepth = getDepth(sampleUv);
			return vec3(sampleUv, sampleSceneDepth);
		}

		// https://blog.demofox.org/2022/01/01/interleaved-gradient-noise-a-different-kind-of-low-discrepancy-sequence/
		float randf(int x, int y) {
			return mod(52.9829189 * mod(0.06711056 * float(x) + 0.00583715 * float(y), 1.0), 1.0);
		}

		// https://graphics.stanford.edu/%7Eseander/bithacks.html
		uint bitCount(uint value) {
			value = value - ((value >> 1u) & 0x55555555u);
			value = (value & 0x33333333u) + ((value >> 2u) & 0x33333333u);
			return ((value + (value >> 4u) & 0xF0F0F0Fu) * 0x1010101u) >> 24u;
		}

		// https://cdrinmatane.github.io/posts/ssaovb-code/
		const uint sectorCount = 32u;
		uint updateSectors(float minHorizon, float maxHorizon, uint outBitfield) {
			uint startBit = uint(minHorizon * float(sectorCount));
			uint horizonAngle = uint(round((maxHorizon - minHorizon) * float(sectorCount)));
			uint angleBit = horizonAngle > 0u ? uint(0xFFFFFFFFu >> (sectorCount - horizonAngle)) : 0u;
			uint currentBitfield = angleBit << startBit;
			return outBitfield | currentBitfield;
		}

		// get indirect lighting and ambient occlusion
		void main() {
			uint indirect = 0u;
			uint occlusion = 0u;

			float visibility = 0.0;
			vec3 lighting = vec3(0.0);
			vec2 frontBackHorizon = vec2(0.0);
			//vec2 aspect = screenSize.yx / screenSize.x;
			//vec3 position = texture(screenPosition, fragUV).rgb
			vec2 aspect = vec2(cameraProjectionMatrix[1][1] / cameraProjectionMatrix[0][0], 1.0);
			float depth = getDepth(vUv.xy);
			if (depth >= 1.0) { discard; return; }
			//vec3 position = getWorldPosition(vUv, depth);
			//vec3 cameraPos = cameraWorldMatrix[3].xyz;
			//vec3 cameraDir = -cameraWorldMatrix[2].xyz;
			//vec3 camera = -normalize(cameraPos - position);
			//vec3 normal = getWorldNormal(vUv.xy); //normalize(texture(tNormal, vUv.xy).rgb);

			vec3 position = getViewPosition(vUv, depth);
			vec3 camera = normalize(-position);
			vec3 normal = getViewNormal(vUv.xy); //normalize(texture(tNormal, vUv.xy).rgb);

			float sliceRotation = twoPi / (sliceCount - 1.0);
			float sampleScale = (-radius * cameraProjectionMatrix[0][0]) / position.z;
			float sampleOffset = 0.01 * scale;
			float jitter = randf(int(gl_FragCoord.x), int(gl_FragCoord.y)) - 0.5;

			for (float slice = 0.0; slice < sliceCount + 0.5; slice += 1.0) {
				float phi = sliceRotation * (slice + jitter) + pi;
				vec2 omega = vec2(cos(phi), sin(phi));
				vec3 direction = directionSwizzle ? vec3(-omega.y, 0.0, -omega.x) : vec3(omega, 0.0);
				vec3 orthoDirection = direction - dot(direction, camera) * camera;
				vec3 axis = cross(direction, camera);
				vec3 projNormal = normal - axis * dot(normal, axis);
				float projLength = length(projNormal);

				float signN = sign(dot(orthoDirection, projNormal));
				float cosN = clamp(dot(projNormal, camera) / projLength, 0.0, 1.0);
				float n = signN * acos(cosN);

				for (float currentSample = 0.0; currentSample < sampleCount + 0.5; currentSample += 1.0) {
					float sampleStep = (currentSample + jitter) / sampleCount + sampleOffset;
					vec2 sampleUV = vUv.xy - sampleStep * sampleScale * omega * aspect;
					vec3 samplePosition = getViewPosition(sampleUV, getDepth(sampleUV));
					vec3 sampleNormal = normalize(texture(tNormal, sampleUV).rgb);
					vec3 sampleLight = texture(tColor, sampleUV).rgb;
					vec3 sampleDistance = samplePosition - position;
					float sampleLength = length(sampleDistance);
					vec3 sampleHorizon = sampleDistance / sampleLength;

					frontBackHorizon.x = dot(sampleHorizon, camera);
					frontBackHorizon.y = dot(normalize(sampleDistance - camera * thickness), camera);

					frontBackHorizon = acos(frontBackHorizon);
					frontBackHorizon = clamp((frontBackHorizon + n + halfPi) / pi, 0.0, 1.0);

					indirect = updateSectors(frontBackHorizon.x, frontBackHorizon.y, 0u);
					lighting += (1.0 - float(bitCount(indirect & ~occlusion)) / float(sectorCount)) *
						sampleLight * clamp(dot(normal, sampleHorizon), 0.0, 1.0) *
						clamp(dot(sampleNormal, -sampleHorizon), 0.0, 1.0);
					occlusion |= indirect;
				}
				visibility += 1.0 - float(bitCount(occlusion)) / float(sectorCount);
			}

			visibility /= sliceCount;
			lighting /= sliceCount;

			//lighting += texture(tColor, vUv.xy).rgb;
			gl_FragColor = vec4(visibility, visibility, visibility, 1.0);// lighting, visibility); //
		}`

};

const SSILVBDepthShader = {

	name: 'SSILVBDepthShader',

	defines: {
		PERSPECTIVE_CAMERA: 1
	},

	uniforms: {
		tDepth: { value: null },
		cameraNear: { value: null },
		cameraFar: { value: null },
	},

	vertexShader: /* glsl */`
		varying vec2 vUv;

		void main() {
			vUv = uv;
			gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
		}`,

	fragmentShader: /* glsl */`
		uniform sampler2D tDepth;
		uniform float cameraNear;
		uniform float cameraFar;
		varying vec2 vUv;

		#include <packing>

		float getLinearDepth( const in vec2 screenPosition ) {
			#if PERSPECTIVE_CAMERA == 1
				float fragCoordZ = texture2D( tDepth, screenPosition ).x;
				float viewZ = perspectiveDepthToViewZ( fragCoordZ, cameraNear, cameraFar );
				return viewZToOrthographicDepth( viewZ, cameraNear, cameraFar );
			#else
				return texture2D( tDepth, screenPosition ).x;
			#endif
		}

		void main() {
			float depth = getLinearDepth( vUv );
			gl_FragColor = vec4( vec3( 1.0 - depth ), 1.0 );

		}`

};

const SSILVBBlendShader = {

	name: 'SSILVBBlendShader',

	uniforms: {
		tDiffuse: { value: null },
		intensity: { value: 1.0 }
	},

	vertexShader: /* glsl */`
		varying vec2 vUv;

		void main() {
			vUv = uv;
			gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
		}`,

	fragmentShader: /* glsl */`
		uniform float intensity;
		uniform sampler2D tDiffuse;
		varying vec2 vUv;

		void main() {
			vec4 texel = texture2D( tDiffuse, vUv );
			gl_FragColor = vec4(mix(vec3(1.), texel.rgb, intensity), texel.a);
		}`

};


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


export { generateMagicSquareNoise, SSILVBShader, SSILVBDepthShader, SSILVBBlendShader };
