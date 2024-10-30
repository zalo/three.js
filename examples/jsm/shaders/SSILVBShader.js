import {
	DataTexture,
	Matrix4,
	RepeatWrapping,
	Vector2,
	Vector3,
	GLSL3
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
		SLICES: 4,
	},

	uniforms: {
		tNormal: { value: null },
		tDepth: { value: null },
		tColor: { value: null },
		tNoise: { value: null },
		iResolution: { value: new Vector2() },
		cameraNear: { value: null },
		cameraFar: { value: null },
		cameraProjectionMatrix: { value: new Matrix4() },
		cameraProjectionMatrixInverse: { value: new Matrix4() },
		cameraWorldMatrix: { value: new Matrix4() },
		cameraWorldMatrixInverse: { value: new Matrix4() },
		radius: { value: 12.0 },
		distanceExponent: { value: 1.7 },
		thickness: { value: 0.5 },
		scale: { value: 1. },
		sceneBoxMin: { value: new Vector3( - 1, - 1, - 1 ) },
		sceneBoxMax: { value: new Vector3( 1, 1, 1 ) },
	},

	glslVersion: GLSL3,

	vertexShader: /* glsl */`

		varying vec2 vUv;

		void main() {
			vUv = uv;
			gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
		}`,

	fragmentShader: /* glsl */`
		// Adapted from "Screen Space Indirect Lighting with Visibility Bitmask" by Olivier Therrien, et al.
		// https://cdrinmatane.github.io/posts/cgspotlight-slides/

		#define MAX_RAY 32u
		#define isPerspectiveCam true
		#define GTVBAO_SLICE_SAMPLING_MODE 3

		#if 1
			// use high quality variants (so there is no unintentional bias when comparing to reference)
			#define USE_HQ_APPROX_SLICE_IMPORTANCE_SAMPLING
			#define USE_HQ_ACOS
		#endif

		//#define RAY_MARCH_SAMPLE_COUNT 32.0
		//#define RAY_MARCH_RADIUS 512.0
		#define USE_UNIFORM_HEMISHPHERE_WEIGHTING false

		#define nearZ 1.0 //0.125 // 1.0 ?

		precision highp float;
		precision highp sampler2D;

		varying vec2 vUv;
		uniform sampler2D tNormal;
		uniform sampler2D tDepth;
		uniform sampler2D tColor;
		uniform sampler2D tNoise;

		uniform vec2 iResolution;
		uniform float cameraNear;
		uniform float cameraFar;
		uniform mat4 cameraProjectionMatrix;
		uniform mat4 cameraProjectionMatrixInverse;		
		uniform mat4 cameraWorldMatrix;
		uniform mat4 cameraWorldMatrixInverse;
		uniform float radius;
		uniform float distanceExponent;
		uniform float thickness;
		uniform float scale;
		uniform bool useCorrectNormals;
		uniform vec2 _ScreenParams;
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

		// ====== View <-> World ====== //
		vec3 VPos_from_WPos(vec3 wpos) {
			vec3 vpos = (cameraWorldMatrixInverse * vec4(wpos, 1.0)).xyz;
			vpos.z *= -1.0;
			return vpos;
		}

		vec3 VVec_from_WVec(vec3 wvec) {
			vec3 vvec = normalize((cameraWorldMatrixInverse * vec4(wvec, 0.0)).xyz);
			vvec.z *= -1.0;
			return vvec;
		}

		// ====== Screen <-> View ====== //
		vec3 SPos_from_VPos(vec3 vpos) {
			vpos.z *= -1.0;
			vec3 spos = getSceneUvAndDepth(vpos);
			
			return vec3(spos.xy * iResolution.xy, spos.z);
		}

		vec3 VPos_from_SPos(vec3 spos) {
			vec3 vpos = getViewPosition(spos.xy / iResolution.xy, spos.z);
			vpos.z *= -1.0;
			return vpos;
		}

		// https://blog.demofox.org/2022/01/01/interleaved-gradient-noise-a-different-kind-of-low-discrepancy-sequence/
		float randf(int x, int y) {
			return mod(52.9829189 * mod(0.06711056 * float(x) + 0.00583715 * float(y), 1.0), 1.0);
		}

		// From http://byteblacksmith.com/improvements-to-the-canonical-one-liner-glsl-rand-for-opengl-es-2-0/
		float rand2(vec2 co) {
			float a = 12.9898;
			float b = 78.233;
			float c = 43758.5453;
			float dt = dot(co.xy, vec2(a, b));
			float sn = mod(dt, 3.14);
			return fract(sin(sn) * c);
		}

		vec2 GTAOFastAcos(vec2 x) {
			vec2 outVal = -0.156583 * abs(x) + halfPi;
			outVal *= sqrt(1.0 - abs(x));
			//return x >= 0.0 ? outVal : pi - outVal; // uhhh does this really work in HLSL?
			return vec2(x.x >= 0.0 ? outVal.x : pi - outVal.x, 
						x.y >= 0.0 ? outVal.y : pi - outVal.y);
		}

		float GTAOFastAcos(float x) {
			float outVal = -0.156583 * abs(x) + halfPi;
			outVal *= sqrt(1.0 - abs(x));
			return x >= 0.0 ? outVal : pi - outVal;
		}

		// https://graphics.stanford.edu/%7Eseander/bithacks.html
		uint bitCount(uint value) {
			value = value - ((value >> 1u) & 0x55555555u);
			value = (value & 0x33333333u) + ((value >> 2u) & 0x33333333u);
			return ((value + (value >> 4u) & 0xF0F0F0Fu) * 0x1010101u) >> 24u;
		}

		// From http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
		// All components are in the range [0…1], including hue.
		vec3 RgbToHsv(vec3 c) {
			vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
			vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
			vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

			float d = q.x - min(q.w, q.y);
			float e = 1.0e-10;
			return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
		}
		vec3 HsvToRgb(vec3 c) {
			vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
			vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
			return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
		}

		// Approximates luminance from an RGB value
		// https://github.com/mrdooz/kumi/blob/master/effects/luminance.hlsl
		float Luminance(vec3 color) {
			return dot(color, vec3(0.299f, 0.587f, 0.114f));
		}
		
		// interleaved gradient noise | license: unclear
		// Jorge Jimenez http://www.iryoku.com/next-generation-post-processing-in-call-of-duty-advanced-warfare
		float IGN(vec2 uv) { return fract(52.9829189 * fract(dot(uv, vec2(0.06711056, 0.00583715)))); }

		float IGN(vec2 uv, uint frame)
		{
			frame = frame % 64u;
			
			uv += 5.588238 * float(frame);
			
			return IGN(uv);
		}

		// linearizes uv using a Hilbert curve; tile dimension = 2^N
		uint EvalHilbertCurve(uvec2 uv, uint N)
		{
			uint C = 0xB4361E9Cu;// cost lookup
			uint P = 0xEC7A9107u;// pattern lookup
			
			uint c = 0u;// accumulated cost
			uint p = 0u;// current pattern

			for(uint i = N; --i < N;)
			{
				uvec2 m = (uv >> i) & 1u;// local uv

				uint n = m.x ^ (m.y << 1u);// linearized local uv

				uint o = (p << 3u) ^ (n << 1u);// offset into lookup tables

				c += ((C >> o) & 3u) << (i << 1u);// accu cost (scaled by level)

				p = (P >> o) & 3u;// update pattern
			}
			
			return c;
		}

		////////////////////////////////////////////////////////////////////////////////////////////////////////////// low-discrepancy sobol noise
		//==========================================================================================================//
		// "Shuffled Scrambled Sobol (2D)" - https://www.shadertoy.com/view/3lcczS | license: unclear
		//  code taken from "Practical Hash-based Owen Scrambling" - http://www.jcgt.org/published/0009/04/01/
		uint reverse_bits(uint x) 
		{
			x = (((x & 0xaaaaaaaau) >> 1) | ((x & 0x55555555u) << 1));
			x = (((x & 0xccccccccu) >> 2) | ((x & 0x33333333u) << 2));
			x = (((x & 0xf0f0f0f0u) >> 4) | ((x & 0x0f0f0f0fu) << 4));
			x = (((x & 0xff00ff00u) >> 8) | ((x & 0x00ff00ffu) << 8));
			
			return ((x >> 16) | (x << 16));
		}

		// license: unclear
		uint laine_karras_permutation(uint x, uint seed) 
		{
			x += seed;
			x ^= x*0x6c50b47cu;
			x ^= x*0xb82f1e52u;
			x ^= x*0xc7afe638u;
			x ^= x*0x8d22f6e6u;
			
			return x;
		}

		// license: unclear
		uint nested_uniform_scramble(uint x, uint seed) 
		{
			x = reverse_bits(x);
			x = laine_karras_permutation(x, seed);
			x = reverse_bits(x);
			
			return x;
		}

		// from https://www.shadertoy.com/view/3ldXzM | license: unclear
		uvec2 sobol_2d(uint index) 
		{
			uvec2 p = uvec2(0u);
			uvec2 d = uvec2(0x80000000u);

			for(; index != 0u; index >>= 1u) 
			{
				if((index & 1u) != 0u) 
				{
					p ^= d;
				}

				d.x >>= 1u;  // 1st dimension Sobol matrix, is same as base 2 Van der Corput
				d.y ^= d.y >> 1u; // 2nd dimension Sobol matrix
			}
			
			return p;
		}

		// license: unclear
		uvec2 shuffled_scrambled_sobol_2d(uint index, uint seed) 
		{
			index = nested_uniform_scramble(index, seed);
			
			uvec2 p = sobol_2d(index);
			
			seed = seed * 2891336453u + 1u;
			p.x = nested_uniform_scramble(p.x, seed );
			seed = seed * 2891336453u + 1u;
			p.y = nested_uniform_scramble(p.y, seed);
		
			return p;
		}

		uint shuffled_scrambled_sobol_angle01(uint x, uint seed) 
		{
			x = reverse_bits(x);
			
			x = laine_karras_permutation(x, seed);
			
			return x;
		}
		//==========================================================================================================//
		//////////////////////////////////////////////////////////////////////////////////////////////////////////////


		////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// RNG
		//==============================================================================================================================================//
		uint  asuint2(float x) { return x == 0.0 ? 0u : floatBitsToUint(x); }
		uvec2 asuint2(vec2 x) { return uvec2(asuint2(x.x ), asuint2(x.y)); }
		uvec3 asuint2(vec3 x) { return uvec3(asuint2(x.xy), asuint2(x.z)); }
		uvec4 asuint2(vec4 x) { return uvec4(asuint2(x.xy), asuint2(x.zw)); }

		float Float01(uint x) { return float(    x ) * (1.0 / 4294967296.0); }
		float Float11(uint x) { return float(int(x)) * (1.0 / 2147483648.0); }

		vec2 Float01(uvec2 x) { return vec2(      x ) * (1.0 / 4294967296.0); }
		vec2 Float11(uvec2 x) { return vec2(ivec2(x)) * (1.0 / 2147483648.0); }

		vec3 Float01(uvec3 x) { return vec3(      x ) * (1.0 / 4294967296.0); }
		vec3 Float11(uvec3 x) { return vec3(ivec3(x)) * (1.0 / 2147483648.0); }

		vec4 Float01(uvec4 x) { return vec4(      x ) * (1.0 / 4294967296.0); }
		vec4 Float11(uvec4 x) { return vec4(ivec4(x)) * (1.0 / 2147483648.0); }

		const float Pi = 3.14159265359;
		const float RcpPi = 1.0 / Pi;
		const float Pi05 = Pi * 0.5;
		const float RcpPi05 = 1.0 / Pi05;

		float Pow2(float x) {return x*x;}
		float Pow3(float x) {return x*x*x;}
		float Pow4(float x) {return Pow2(Pow2(x));}

		// http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/
		// https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/
		const float rPhif1 =      0.6180340;
		const vec2  rPhif2 = vec2(0.7548777, 0.5698403);
		const vec3  rPhif3 = vec3(0.8191725, 0.6710436, 0.5497005);
		const vec4  rPhif4 = vec4(0.8566749, 0.7338919, 0.6287067, 0.5385973);

		const uint  rPhi1 =       2654435769u;
		const uvec2 rPhi2 = uvec2(3242174889u, 2447445413u);
		const uvec3 rPhi3 = uvec3(3518319153u, 2882110345u, 2360945575u);
		const uvec4 rPhi4 = uvec4(3679390609u, 3152041523u, 2700274805u, 2313257605u);

		// low bias version | https://nullprogram.com/blog/2018/07/31/ | license: public domain (http://unlicense.org/)
		uint WellonsHash(uint x)
		{
			x ^= x >> 16u;
			x *= 0x7feb352dU;
			x ^= x >> 15u;
			x *= 0x846ca68bU;
			x ^= x >> 16u;

			return x;
		}

		// minimal bias version | https://nullprogram.com/blog/2018/07/31/ | license: public domain (http://unlicense.org/)
		uint WellonsHash2(uint x)
		{
			x ^= x >> 17u;
			x *= 0xed5ad4bbU;
			x ^= x >> 11u;
			x *= 0xac4c1b51U;
			x ^= x >> 15u;
			x *= 0x31848babU;
			x ^= x >> 14u;

			return x;
		}

		// http://marc-b-reynolds.github.io/math/2016/03/29/weyl_hash.html | license: public domain (http://unlicense.org/)
		uint WeylHash(uvec2 c)
		{ 
			return ((c.x * 0x3504f333u) ^ (c.y * 0xf1bbcdcbu)) * 741103597u;
		}

		// Pierre L'Ecuyer - "TABLES OF LINEAR CONGRUENTIAL GENERATORS OF DIFFERENT SIZES AND GOOD LATTICE STRUCTURE"
		// https://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-00996-5/S0025-5718-99-00996-5.pdf
		const uint lcgM = 2891336453u;// ideal for 32 bits with odd c

		uint lcg(uint h)
		{
			return h * lcgM + 0x5C995C6Du;
		}

		#define SEED uvec4(0x5C995C6Du, 0x6A3C6A57u, 0xC65536CBu, 0x3563995Fu)

		// Melissa E. O'Neill - "PCG: A Family of Simple Fast Space-Efficient Statistically Good Algorithms for Random Number Generation"
		// https://www.cs.hmc.edu/tr/hmc-cs-2014-0905.pdf

		// Mark Jarzynski & Marc Olano - "Hash Functions for GPU Rendering"
		// http://jcgt.org/published/0009/03/02/ | https://www.shadertoy.com/view/XlGcRh
		uvec3 pcg3Mix(uvec3 h)
		{
			h.x += h.y * h.z; 
			h.y += h.z * h.x; 
			h.z += h.x * h.y;
			
			return h;
		}

		uvec3 pcg3Permute(uvec3 h)
		{
			h = pcg3Mix(h);

			h ^= h >> 16u;
			
			return pcg3Mix(h);
		}

		uvec3 pcg3(inout uint state)
		{
			state = lcg(state);

			return pcg3Permute(uvec3(2447445413u, state, 3242174889u));
		}

		uvec3 pcg3(uvec3 h, uint seed)
		{
			uvec3 c = (seed << 1u) ^ SEED.xyz;
			
			return pcg3Permute(h * lcgM + c);
		}

		uvec4 pcg4Mix(uvec4 h)
		{
			h.x += h.y * h.w; 
			h.y += h.z * h.x; 
			h.z += h.x * h.y;
			h.w += h.y * h.z;
			
			return h;
		}

		uvec4 pcg4Permute(uvec4 h)
		{
			h = pcg4Mix(h);

			h ^= h >> 16u;
			
			return pcg4Mix(h);
		}

		uvec4 pcg4(inout uint state)
		{
			state = lcg(state);

			return pcg4Permute(uvec4(2882110345u, state, 3518319153u, 2360945575u));
		}

		uvec4 pcg4(uvec4 h, uint seed)
		{
			uvec4 c = (seed << 1u) ^ SEED;

			return pcg4Permute(h * lcgM + c);
		}

		uint pcg(inout uint state)
		{
			state = lcg(state);
			
			uint word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
			
			return (word >> 22u) ^ word;
		}

		uint pcg(uint h, uint seed)
		{
			uint c = (seed << 1u) ^ SEED.x;

			h = h * lcgM + c;
			
			h = ((h >> ((h >> 28u) + 4u)) ^ h) * 277803737u;
			
			return (h >> 22u) ^ h;
		}


		
		float ACos_Approx(float x) {
			float v = x < 0.0 ? Pi : 0.0;
			
			x = abs(x);

			// minimizes max abs(ACos_Approx(cos(x)) - x)
			float s = 0.21545;
			float s1 = -(s + 1.0);
			
			float u = (x * s + s1) * x + 1.0;
			
			float ang = abs(v - Pi05 * sqrt(clamp(u, 0.0, 1.0)));
			
			return ang;
		}

		uint  Hash(uint  h, uint seed) { return pcg(h, seed); }
		uvec2 Hash(uvec2 h, uint seed) { return pcg3(uvec3(h, 0u), seed).xy; }
		uvec3 Hash(uvec3 h, uint seed) { return pcg3(h, seed); }
		uvec4 Hash(uvec4 h, uint seed) { return pcg4(h, seed); }

		vec4 Hash01x4(vec4  v, uint seed) { return Float01(pcg4(asuint2(v), seed)); }
		vec4 Hash01x4(vec3  v, uint seed) { return Hash01x4(vec4(v, 0.0          ), seed); }
		vec4 Hash01x4(vec2  v, uint seed) { return Hash01x4(vec4(v, 0.0, 0.0     ), seed); }
		vec4 Hash01x4(float v, uint seed) { return Hash01x4(vec4(v, 0.0, 0.0, 0.0), seed); }

		vec3 Hash01x3(vec4  v, uint seed) { return Float01(pcg4(asuint2(v), seed).xyz); }
		vec3 Hash01x3(vec3  v, uint seed) { return Float01(pcg3(asuint2(v), seed)); }
		vec3 Hash01x3(vec2  v, uint seed) { return Hash01x3(vec3(v, 0.0     ), seed); }
		vec3 Hash01x3(float v, uint seed) { return Hash01x3(vec3(v, 0.0, 0.0), seed); }

		vec2 Hash01x2(vec4  v, uint seed) { return Float01(pcg4(asuint2(v), seed).xy); }
		vec2 Hash01x2(vec3  v, uint seed) { return Float01(pcg3(asuint2(v), seed).xy); }
		vec2 Hash01x2(vec2  v, uint seed) { return Hash01x3(vec3(v, 0.0     ), seed).xy; }
		vec2 Hash01x2(float v, uint seed) { return Hash01x3(vec3(v, 0.0, 0.0), seed).xy; }

		float Hash01(vec4  v, uint seed) { return Float01(pcg4(asuint2(v), seed).x); }
		float Hash01(vec3  v, uint seed) { return Float01(pcg3(asuint2(v), seed).x); }
		float Hash01(vec2  v, uint seed) { return Float01(pcg3(asuint2(vec3(v, 0.0)), seed).x); }
		float Hash01(float v, uint seed) { return Float01(pcg(asuint2(v), seed)); }


		vec4 Hash11x4(vec4  v, uint seed) { return Float11(pcg4(asuint2(v), seed)); }
		vec4 Hash11x4(vec3  v, uint seed) { return Hash11x4(vec4(v, 0.0          ), seed); }
		vec4 Hash11x4(vec2  v, uint seed) { return Hash11x4(vec4(v, 0.0, 0.0     ), seed); }
		vec4 Hash11x4(float v, uint seed) { return Hash11x4(vec4(v, 0.0, 0.0, 0.0), seed); }

		vec3 Hash11x3(vec4  v, uint seed) { return Float11(pcg4(asuint2(v), seed).xyz); }
		vec3 Hash11x3(vec3  v, uint seed) { return Float11(pcg3(asuint2(v), seed)); }
		vec3 Hash11x3(vec2  v, uint seed) { return Hash11x3(vec3(v, 0.0     ), seed); }
		vec3 Hash11x3(float v, uint seed) { return Hash11x3(vec3(v, 0.0, 0.0), seed); }

		vec2 Hash11x2(vec4  v, uint seed) { return Float11(pcg4(asuint2(v), seed).xy); }
		vec2 Hash11x2(vec3  v, uint seed) { return Float11(pcg3(asuint2(v), seed).xy); }
		vec2 Hash11x2(vec2  v, uint seed) { return Hash11x3(vec3(v, 0.0     ), seed).xy; }
		vec2 Hash11x2(float v, uint seed) { return Hash11x3(vec3(v, 0.0, 0.0), seed).xy; }

		float Hash11(vec4  v, uint seed) { return Float11(pcg4(asuint2(v), seed).x); }
		float Hash11(vec3  v, uint seed) { return Float11(pcg3(asuint2(v), seed).x); }
		float Hash11(vec2  v, uint seed) { return Float11(pcg3(asuint2(vec3(v, 0.0)), seed).x); }
		float Hash11(float v, uint seed) { return Float11(pcg(asuint2(v), seed)); }


		vec4 Hash01x4(uvec4 v, uint seed) { return Float01(pcg4(v, seed)); }
		vec4 Hash01x4(uvec3 v, uint seed) { return Hash01x4(uvec4(v, 0u        ), seed); }
		vec4 Hash01x4(uvec2 v, uint seed) { return Hash01x4(uvec4(v, 0u, 0u    ), seed); }
		vec4 Hash01x4(uint  v, uint seed) { return Hash01x4(uvec4(v, 0u, 0u, 0u), seed); }

		vec3 Hash01x3(uvec4 v, uint seed) { return Float01(pcg4(v, seed).xyz); }
		vec3 Hash01x3(uvec3 v, uint seed) { return Float01(pcg3(v, seed)); }
		vec3 Hash01x3(uvec2 v, uint seed) { return Hash01x3(uvec3(v, 0u    ), seed); }
		vec3 Hash01x3(uint  v, uint seed) { return Hash01x3(uvec3(v, 0u, 0u), seed); }

		vec2 Hash01x2(uvec4 v, uint seed) { return Float01(pcg4(v, seed).xy); }
		vec2 Hash01x2(uvec3 v, uint seed) { return Float01(pcg3(v, seed).xy); }
		vec2 Hash01x2(uvec2 v, uint seed) { return Hash01x3(uvec3(v, 0u    ), seed).xy; }
		vec2 Hash01x2(uint  v, uint seed) { return Hash01x3(uvec3(v, 0u, 0u), seed).xy; }

		float Hash01(uvec4 v, uint seed) { return Float01(pcg4(v, seed).x); }
		float Hash01(uvec3 v, uint seed) { return Float01(pcg3(v, seed).x); }
		float Hash01(uvec2 v, uint seed) { return Float01(pcg3(uvec3(v, 0u), seed).x); }
		float Hash01(uint  v, uint seed) { return Float01(pcg(v, seed)); }


		vec4 Hash11x4(uvec4 v, uint seed) { return Float11(pcg4(v, seed)); }
		vec4 Hash11x4(uvec3 v, uint seed) { return Hash11x4(uvec4(v, 0u        ), seed); }
		vec4 Hash11x4(uvec2 v, uint seed) { return Hash11x4(uvec4(v, 0u, 0u    ), seed); }
		vec4 Hash11x4(uint  v, uint seed) { return Hash11x4(uvec4(v, 0u, 0u, 0u), seed); }

		vec3 Hash11x3(uvec4 v, uint seed) { return Float11(pcg4(v, seed).xyz); }
		vec3 Hash11x3(uvec3 v, uint seed) { return Float11(pcg3(v, seed)); }
		vec3 Hash11x3(uvec2 v, uint seed) { return Hash11x3(uvec3(v, 0u    ), seed); }
		vec3 Hash11x3(uint  v, uint seed) { return Hash11x3(uvec3(v, 0u, 0u), seed); }

		vec2 Hash11x2(uvec4 v, uint seed) { return Float11(pcg4(v, seed).xy); }
		vec2 Hash11x2(uvec3 v, uint seed) { return Float11(pcg3(v, seed).xy); }
		vec2 Hash11x2(uvec2 v, uint seed) { return Hash11x3(uvec3(v, 0u    ), seed).xy; }
		vec2 Hash11x2(uint  v, uint seed) { return Hash11x3(uvec3(v, 0u, 0u), seed).xy; }

		float Hash11(uvec4 v, uint seed) { return Float11(pcg4(v, seed).x); }
		float Hash11(uvec3 v, uint seed) { return Float11(pcg3(v, seed).x); }
		float Hash11(uvec2 v, uint seed) { return Float11(pcg3(uvec3(v, 0u), seed).x); }
		float Hash11(uint  v, uint seed) { return Float11(pcg(v, seed)); }


		#ifdef USE_HQ_ACOS
		float ACos(float x) {   
			return acos(clamp(x, -1.0, 1.0));
		}
		#else
		float ACos(float x) {
			return ACos_Approx(x);
		}
		#endif

		vec2 ACos(vec2 v) {
			return vec2(ACos(v.x), ACos(v.y));
		}

		//==================================================================================//
		//////////////////////////////////////////////////////////////////////////////////////


		////////////////////////////////////////////////////////////////////////////////////// slice sampling
		//==================================================================================//

		#if 1
		// vvsN: view vec space normal | rnd01: [0, 1]
		vec2 SampleSliceDir(vec3 vvsN, float rnd01) {
			float ang0 = rnd01 * Pi;

			vec2 dir0 = vec2(cos(ang0), sin(ang0));
					
			float l = length(vvsN.xy);

			if(l == 0.0) return dir0;
			
			// flip dir0 into hemi-circle of rsN.xy
			dir0 *= dot(dir0, vvsN.xy) < 0.0 ? -1.0 : 1.0;
			
			vec2 n = vvsN.xy / l;
			
			// SampleSlice(..) inlined + optimized
			vec2 dir;
			{
				float x = dir0.x * n.y - dir0.y * n.x;// dir0 x n
				
				float s = l;// acos stretch param; chosen to approx match ref pdf
				{
					// quadratic bias
					s += (s - s * s) * 0.15;
				}
				
			#ifdef USE_HQ_APPROX_SLICE_IMPORTANCE_SAMPLING
				float y = acos(x * sin(s * Pi05)) * RcpPi05;// stretched acos
			#else
				// approximation (not that much faster)
				float k = 0.21545;// asin approx param
				
				float xs;// inverse approx asin
				{
					float a = 0.5 + 0.5 / k;
					float b = 0.5 - 0.5 / k;
					float d = b * b;
					float c = 4.0 / k;

					xs = 0.5 - 0.5 * s;
					xs = a - sqrt(d + c * (xs*xs));
				}
				
				x *= xs;// stretch curve along x
				
				float y;// approx acos
				{
					float v = x > 0.0 ? 2.0 : 0.0;

					float g = -k - 1.0;

					float u = (abs(x) * k + g) * abs(x) + 1.0;

					y = abs(v - sqrt(clamp(u, 0.0, 1.0)));        
				}
			#endif
				
				float ys = 1.0 / s;// remap curve along y
				
				dir.y = ys - ys * y;// [-1, 1]
				dir.x = sqrt(clamp(1.0 - dir.y*dir.y, 0.0, 1.0));// [0, 1]
			}    
			
			// align x-axis with n
			return vec2(dir.x * n.x - dir.y * n.y, 
						dir.y * n.x + dir.x * n.y);
		}

		#else

		float QBias(float x, float b) {
			return x + (x - x*x) * b;
		}

		float SinStep(float x) {
			return 0.5 - 0.5 * cos(x * Pi);
		}

		float InvSinStep(float x) {
			return acos(1.0 - 2.0 * x) * RcpPi;
		}

		float InvSinStep(float x, float s) {
			if(s < 0.00001) return x;
			
			float u = asin(sin(s * Pi05) * (1.0 - 2.0 * x));
			
			return 0.5 - u * (RcpPi / s);
		}

		float SampleSlice(float x, float sinNV) {
			float s = QBias(sinNV, 0.15);
			
				x =    SinStep(x);
			float y = InvSinStep(x, s);
				y = InvSinStep(y);
			
			return y;
		}

		vec2 cmul(vec2 c0, vec2 c1) {
			return vec2(c0.x * c1.x - c0.y * c1.y, 
						c0.y * c1.x + c0.x * c1.y);
		}

		// vvsN: view vec space normal | rnd01: [0, 1]
		vec2 SampleSliceDir(vec3 vvsN, float rnd01) {
			float ang0 = rnd01 * Pi;
			
			vec2 dir0 = vec2(cos(ang0), sin(ang0));

			float l = length(vvsN.xy);

			if(l == 0.0) return dir0;
			
			// flip dir0 into hemi-circle of rsN.xy
			dir0 *= dot(dir0, vvsN.xy) < 0.0 ? -1.0 : 1.0;
			
			vec2 n = vvsN.xy / l;    
			
			// align n with x-axis
			dir0 = cmul(dir0, n * vec2(1.0, -1.0));

			// sample slice angle
			float ang;
			{
				float x = atan(-dir0.y / dir0.x) * RcpPi + 0.5;
				float sinNV = l;

				ang = SampleSlice(x, sinNV) * Pi - Pi05;
			}
			
			// ray space slice direction
			vec2 dir = vec2(cos(ang), sin(ang));
			
			// align x-axis with n
			dir = cmul(dir, n);
			
			return dir;
		}
		#endif

		//==================================================================================//
		//////////////////////////////////////////////////////////////////////////////////////


		////////////////////////////////////////////////////////////////////////////////////// quaternion utils
		//==================================================================================//

		vec4 GetQuaternion(vec3 from, vec3 to) {
			vec3 xyz = cross(from, to);
			float s  =   dot(from, to);

			float u = inversesqrt(max(0.0, s * 0.5 + 0.5));// rcp(cosine half-angle formula)
			
			s    = 1.0 / u;
			xyz *= u * 0.5;

			return vec4(xyz, s);  
		}

		vec4 GetQuaternion(vec3 to) {
			//vec3 from = vec3(0.0, 0.0, 1.0);

			vec3 xyz = vec3(-to.y, to.x, 0.0);// cross(from, to);
			float s  =                   to.z;//   dot(from, to);

			float u = inversesqrt(max(0.0, s * 0.5 + 0.5));// rcp(cosine half-angle formula)
			
			s    = 1.0 / u;
			xyz *= u * 0.5;

			return vec4(xyz, s);  
		}

		// transform v by unit quaternion q.xyzs
		vec3 Transform(vec3 v, vec4 q) {
			vec3 k = cross(q.xyz, v);
			
			return v + 2.0 * vec3(dot(vec3(q.wy, -q.z), k.xzy),
								dot(vec3(q.wz, -q.x), k.yxz),
								dot(vec3(q.wx, -q.y), k.zyx));
		}

		// transform v by unit quaternion q.xy0s
		vec3 Transform_Qz0(vec3 v, vec4 q) {
			float k = v.y * q.x - v.x * q.y;
			float g = 2.0 * (v.z * q.w + k);
			
			vec3 r;
			r.xy = v.xy + q.yx * vec2(g, -g);
			r.z  = v.z  + 2.0 * (q.w * k - v.z * dot(q.xy, q.xy));
			
			return r;
		}

		// transform v.xy0 by unit quaternion q.xy0s
		vec3 Transform_Vz0Qz0(vec2 v, vec4 q) {
			float o = q.x * v.y;
			float c = q.y * v.x;
			
			vec3 b = vec3( o - c,
						-o + c,
						o - c);
			
			return vec3(v, 0.0) + 2.0 * (b * q.yxw);
		}

		//==================================================================================//
		//////////////////////////////////////////////////////////////////////////////////////


		////////////////////////////////////////////////////////////////////////////////////// GT-VBAO
		//==================================================================================//

		// https://graphics.stanford.edu/%7Eseander/bithacks.html#CountBitsSetParallel | license: public domain
		uint CountBits(uint v) {
			v = v - ((v >> 1u) & 0x55555555u);
			v = (v & 0x33333333u) + ((v >> 2u) & 0x33333333u);
			return ((v + (v >> 4u) & 0xF0F0F0Fu) * 0x1010101u) >> 24u;
		}


		float SliceRelCDF_Uniform(float x, float angN) {
			float phi = x * Pi - Pi05;
			
			bool c = phi >= angN;

			float m0 = c ? 2.0 : 0.0;
			float m1 = c ?-1.0 : 1.0;

			float d0 = 0.5 * (m0 + m1 * cos(angN - phi) + sin(angN));
			
			return d0;
		}

		float SliceRelCDF_Cos(float x, float angN) {
			float phi = x * Pi - Pi05;

			bool c = phi >= angN;
			
			float n0 = c ?  3.0 : 1.0;
			float n1 = c ? -1.0 : 1.0;
			float n2 = c ?  4.0 : 0.0;
			
			float t0 = n0 * cos(angN) + n1 * cos(angN - 2.0 * phi) + (n2 * angN + (n1 * 2.0) * phi + Pi) * sin(angN);
			float t1 = 4.0 * (cos(angN) + angN * sin(angN));

			return t0 / t1;
		}


		vec3 GTVBAO(vec2 uv0, vec3 wpos, vec3 N, uint pxId, uint dirCount) {
			//if(doTempAccu) pxId += uint(iFrame) * 98u;// 135u 153u (159u) 169u 193 208 224 242 258u (273u) 276u 279u

			vec3 positionVS = VPos_from_WPos(wpos);
			vec3 normalVS   = VVec_from_WVec(N);
			
			vec3 V = isPerspectiveCam ? -normalize(positionVS) : vec3(0.0, 0.0, -1.0);

			vec2 rayStart = SPos_from_VPos(positionVS).xy;

			//return vec3(rayStart, 0.0) / 1000.0;

			float ao = 0.0;
			
			for(uint i = 0u; i < dirCount; ++i)
			{        
				uint h = pxId * dirCount + i;
				
				////////////////////////////////////////////////// slice direction sampling
				vec3 smplDirVS;// view space sampling vector
				vec2 dir;// screen space sampling vector
				{
				#if GTVBAO_SLICE_SAMPLING_MODE == 1
					// sample slice dir uniformly and later compute slice_weight accordingly
					
					float rnd01 = Float01(h * rPhi1);
					//rnd01 = IGN(floor(uv0), USE_TEMP_ACCU_COND ? uint(iFrame) : 0u);

					dir = vec2(cos(rnd01 * Pi), sin(rnd01 * Pi));

					smplDirVS = vec3(dir.xy, 0.0);

					if(isPerspectiveCam)
					{
						// set up View Vec Space -> View Space mapping
						vec4 Q_toV = GetQuaternion(V);
					
						smplDirVS = Transform_Vz0Qz0(dir, Q_toV);

						vec3 rayStart = SPos_from_VPos(positionVS);
						vec3 rayEnd   = SPos_from_VPos(positionVS + smplDirVS*(nearZ*0.5));

						vec3 rayDir   = rayEnd - rayStart;

						rayDir /= length(rayDir.xy);

						dir = rayDir.xy;
					}
					
				#elif GTVBAO_SLICE_SAMPLING_MODE == 2
					// sample cos lobe in world space and project dir to screen space to be used as slice dir
					
					vec2 s = Float11(shuffled_scrambled_sobol_2d(h, 0xCC925D21u));

					vec3 cosDir = normalize(Sample_Sphere(s) + N);

					smplDirVS = VVec_from_WVec(cosDir);

					vec3 rayDir = smplDirVS;
					
					if(isPerspectiveCam)
					{
						rayDir = SPos_from_WPos(wpos + cosDir * (nearZ * 0.5)) - SPos_from_WPos(wpos);
					}
					
					// 1 px step size
					rayDir /= length(rayDir.xy);

					dir = rayDir.xy;

					// make orthogonal to V (alternatively use sliceN = normalize(sliceN);)
					smplDirVS = normalize(smplDirVS - V * dot(V, smplDirVS));

				#else
					// approximate slice dir importance sampling
					
					float rnd01 = Float01(h * rPhi1);// 'Hilbert R1 Blue Noise' by paniq: https://www.shadertoy.com/view/3tB3z3
					//rnd01 = IGN(floor(uv0), USE_TEMP_ACCU_COND ? uint(iFrame) : 0u);

					// set up View Vec Space <-> View Space mapping
					vec4   Q_toV = GetQuaternion(V);
					vec4 Q_fromV = Q_toV * vec4(vec3(-1.0), 1.0);// conjugate

					vec3 normalVVS = normalVS;

					if(isPerspectiveCam) normalVVS = Transform_Qz0(normalVS, Q_fromV);

					dir = SampleSliceDir(normalVVS, rnd01);

					smplDirVS = vec3(dir.xy, 0.0);

					if(isPerspectiveCam)
					{
						smplDirVS = Transform_Vz0Qz0(dir, Q_toV);

						vec3 rayStart = SPos_from_VPos(positionVS);
						vec3 rayEnd   = SPos_from_VPos(positionVS + smplDirVS*(nearZ*0.5));

						vec3 rayDir   = rayEnd - rayStart;

						rayDir /= length(rayDir.xy);

						dir = rayDir.xy;
					}
				#endif
				
					if(USE_UNIFORM_HEMISHPHERE_WEIGHTING)
					{
						float rnd01 = Float01(h * rPhi1);
		
						dir = vec2(cos(rnd01 * Pi), sin(rnd01 * Pi));
						smplDirVS = vec3(dir, 0.0);
						
					#ifndef USE_ORIGNAL_VBAO_IF_UNIFORM_HEMISHPHERE_WEIGHTING_IS_ACTIVE
						if(isPerspectiveCam)
						{
							// set up View Vec Space -> View Space mapping
							vec4 Q_toV = GetQuaternion(V);
					
							smplDirVS = Transform_Vz0Qz0(dir, Q_toV);

							vec3 rayStart = SPos_from_VPos(positionVS);
							vec3 rayEnd   = SPos_from_VPos(positionVS + smplDirVS*(nearZ*0.5));

							vec3 rayDir   = rayEnd - rayStart;

							rayDir /= length(rayDir.xy);

							dir = rayDir.xy;
						}
					#endif
					}
				}
				//////////////////////////////////////////////////
				
				////////////////////////////////////////////////// construct slice
				float cosN, angN, projNRcpLen;
				{
					vec3 sliceN = cross(V, smplDirVS);

					vec3 projN = normalVS - sliceN * dot(normalVS, sliceN);

					float projNSqrLen = dot(projN, projN);
					if(projNSqrLen == 0.0) return vec3(1.0);

					projNRcpLen = inversesqrt(projNSqrLen);

					cosN = dot(projN, V) * projNRcpLen;

					vec3 T = cross(sliceN, projN);
					
					float sgn = dot(V, T) < 0.0 ? -1.0 : 1.0;
					
					angN = sgn * ACos(cosN);
				}
				//////////////////////////////////////////////////

				vec2 rnd01 = Hash01x2(h, 0x968CC604u);
				
				// find horizons
				uint occBits = 0u;
				for(float d = -1.0; d <= 1.0; d += 2.0)
				{
					vec2 rayDir = dir.xy * d;
					
					const float count = float(SAMPLES);//RAY_MARCH_SAMPLE_COUNT;
					
					float s = pow(radius, 1.0/count); // const RAY_MARCH_RADIUS 
					
					float t = pow(s, rnd01.x);// init t: [1, s]
					
					rnd01.x = 1.0 - rnd01.x;
					
					for (float i = 0.0; i < count; ++i)
					{
						vec2 samplePos = rayStart + rayDir * t;
						
						t *= s;
						
						// handle oob
						if(samplePos.x < 0.0 || samplePos.x >= iResolution.x || samplePos.y < 0.0 || samplePos.y >= iResolution.y) break;
						
						//float sampleDepth = textureLod(iChannel2, samplePos / iResolution.xy, 0.0).w;
						float sampleDepth = getDepth(samplePos / iResolution.xy);
						
						vec3 samplePosVS = VPos_from_SPos(vec3(samplePos, sampleDepth));

						float Thickness = thickness;

						vec3 deltaPosFront = samplePosVS - positionVS;
						vec3 deltaPosBack  = deltaPosFront - V * Thickness;
						
						if(isPerspectiveCam)
						{
						#if 0
							deltaPosBack = VPos_from_SPos(vec3(samplePos, sampleDepth + Thickness)) - positionVS;
						#else
							// also valid, but not consistent with reference ray marcher
							deltaPosBack = deltaPosFront + normalize(samplePosVS) * Thickness;
						#endif
						}

						// project samples onto unit circle and compute angles relative to V
						vec2 horCos = vec2(dot(normalize(deltaPosFront), V), 
										   dot(normalize(deltaPosBack ), V));

						vec2 horAng = ACos(horCos) * d;

						// shift relative angles from V to N + map to [0,1]
						vec2 hor01 = clamp((horAng + angN) * RcpPi + 0.5, 0.0, 1.0);

						// sampling direction flips min/max angles
						hor01 = d >= 0.0 ? hor01.xy : hor01.yx;
						
						if(!USE_UNIFORM_HEMISHPHERE_WEIGHTING)
						{
							// map to slice relative distribution
							hor01.x = SliceRelCDF_Cos(hor01.x, angN);
							hor01.y = SliceRelCDF_Cos(hor01.y, angN);

							// jitter sample locations + clamp01
							hor01 = clamp(hor01 + rnd01.y * (1.0/32.0), 0.0, 1.0);
						}
					#ifndef USE_ORIGNAL_VBAO_IF_UNIFORM_HEMISHPHERE_WEIGHTING_IS_ACTIVE
						else
						{
							// map to slice relative distribution
							hor01.x = SliceRelCDF_Uniform(hor01.x, angN);
							hor01.y = SliceRelCDF_Uniform(hor01.y, angN);

							// jitter sample locations + clamp01
							hor01 = clamp(hor01 + rnd01.y * (1.0/32.0), 0.0, 1.0);
						}
					#endif
					
						uint occBits0;// turn arc into bit mask
						{
							uvec2 horInt = uvec2(floor(hor01 * 32.0));

							uint OxFFFFFFFFu = 0xFFFFFFFFu;// don't inline here! ANGLE bug: https://issues.angleproject.org/issues/353039526

							uint mX = horInt.x < 32u ? OxFFFFFFFFu <<        horInt.x  : 0u;
							uint mY = horInt.y != 0u ? OxFFFFFFFFu >> (32u - horInt.y) : 0u;

							occBits0 = mX & mY;            
						}

						occBits = occBits | occBits0;
					}
				}
				
				float occ0 = float(CountBits(occBits)) * (1.0/32.0);

				float slice_weight = 1.0;
				
			#if GTVBAO_SLICE_SAMPLING_MODE == 1
				// if we sample the slice dir from a uniform distribution we need to account for that here
				slice_weight = 1.0/projNRcpLen * (cosN + angN * sin(angN));
			#endif
				
				ao += (1.0 - occ0) * slice_weight;
			}
			
			ao /= float(dirCount);
			
			return vec3(ao);
		}


		// get indirect lighting and ambient occlusion
		void main() {
			//vec2 aspect = vec2(cameraProjectionMatrix[0][0] / cameraProjectionMatrix[1][1], 1.0);
			float depth = getDepth(vUv.xy);
			if (depth >= 1.0) { discard; return; }

			vec2 uv0 = gl_FragCoord.xy;// / iResolution.xy;
			uvec2 uvu = uvec2(uv0.xy - 0.5);

			// randomly shift noise pattern around
			//if(USE_TEMP_ACCU_COND) uvu += Hash(uvec2(iFrame, 0u), 0xBD1E0BB0u).xy;

			vec3 wpos = getWorldPosition(vUv, depth);
			vec3 N    = normalize(getWorldNormal(vUv.xy));
			wpos     += N * (1.0/1024.0);

			// linearize uv in a locality preserving way
    		uint pxId = EvalHilbertCurve(uvu, 9u);
			//pxId = 0u;

			uint count = uint(SLICES);//SLICE_COUNT;
        	vec3 ssao = GTVBAO(uv0, wpos, N, pxId, count);
			//gl_FragColor = vec4(ssao, 1.0); //vec4(uvu.x, uvu.y, 0.0, 1.0);//vec4(vec3(float(pxId)/1000000.0), 1.0); // vec4(N, 1.0); //
			gl_FragColor = vec4(vec3(saturate(pow(saturate(ssao.x), scale))), 1.0);
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
