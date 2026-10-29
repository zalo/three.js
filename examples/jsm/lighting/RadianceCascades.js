import { BackSide, Box3, Matrix3, Matrix4, Node, NodeUpdateType, StorageBufferAttribute, Vector3 } from 'three/webgpu';
import { Break, Fn, If, Loop, atan, atomicAdd, atomicLoad, atomicStore, cos, cross, dFdx, dFdy, dot, float, floor, fract, hash, instanceIndex, int, length, luminance, min, nodeObject, normalize, screenCoordinate, select, sin, sqrt, storage, uint, uvec2, uvec3, vec2, vec3, vec4 } from 'three/tsl';

import { LightMapper } from './LightMapper.js';

const _a = /*@__PURE__*/ new Vector3();
const _b = /*@__PURE__*/ new Vector3();
const _c = /*@__PURE__*/ new Vector3();
const _matrix = /*@__PURE__*/ new Matrix4();
const _normalMatrix = /*@__PURE__*/ new Matrix3();
const _normal = /*@__PURE__*/ new Vector3();
const _vertexNormal = /*@__PURE__*/ new Vector3();

// fixed point increments of the R2 low-discrepancy sequence, see
// https://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/

const R2 = [ 3242174889, 2447445414 ];

// a probe direction that has no probe or no samples

const NONE = 0xffffffff;

// the deposits are summed as fixed point integers, since WGSL has no float atomics: the radiance in
// 64 bits of two words per channel, which spans from the dim bounces to the sun of a sky, then the
// transmittance and the count of the rays of a bin

const SCALE = 2 ** 24;
const BETA_SCALE = 2 ** 12;
const STRIDE = 8;

// sub-directions per base direction that integrate the cosine of a receiver over it

const COSINE_SAMPLES = 4;

/**
 * Diffuse global illumination with radiance cascades for {@link LightMapper}, following
 * [Split Radiance Cascades]{@link https://arxiv.org/abs/2607.20384} (Freeman, Sannikov and
 * Pitt-Francis, 2026). Probes in a sparse world-space grid near the light-mapped surfaces store the
 * radiance and transmittance of rays in cascades of distance intervals: every cascade doubles the
 * probe spacing, quadruples the directions per probe and the length of its interval, so that close
 * occluders are resolved with a fine spacing and far light with a fine angular resolution.
 *
 * The rays come from a source, which traces them from the texels of the light map: each ray adds
 * its radiance to the cascade whose interval holds its hit, and its transmittance to the cascades
 * below ("ray splitting"). The cascades are merged from the coarsest to the finest one, each texel
 * gathers the irradiance from the interpolated probes of the finest cascade, and the surfaces
 * closer than twice its spacing from its own rays ("cascade -1"), which keeps contact shadows.
 * Sources:
 *
 * - {@link RayTracedRadiosity}, via `rayTracedCascadeSource()`, traces rays through a BVH, see
 * `three-mesh-bvh`, and samples the emitters with shadow rays per texel.
 * - {@link RasterCascadeSource} gathers the nearest surfaces along the rasterized directions of a
 * {@link StochasticRadiosity} solver, which all texels share. It needs no dependencies, but the
 * probes have to accumulate the rays of all samples, see the `accumulate` option, and its light
 * maps then weigh the later samples higher, see the `sampleWeight` option of {@link LightMapper}.
 *
 * ```js
 * const cascades = new RadianceCascades( new RasterCascadeSource( radiosity ), { accumulate: true } );
 * cascades.update( renderer, scene, size );
 *
 * const lightMapper = new LightMapper( renderer, size, radianceCascadesIrradiance( cascades ), { sampleWeight: ( sample ) => sample.add( 1 ) } );
 * ```
 *
 * The probes cover the meshes with light map UVs in `uv1`, the cascades are allocated for them by
 * `update()`. Call it again after changing the scene. Texels share probes with the texels of the same
 * orientation, by the dominant axis of their face normals, so the hidden and opposite sides of nearby
 * surfaces keep their light apart. Each direction of a probe also keeps the luminance-weighted mean
 * direction of its light, so small bright lights keep their cosine on the receivers. The light is
 * biased by the interpolation of the probes: occluders and lights smaller than the spacing of the
 * cascade that sees them are blurred.
 *
 * This class can only be used with {@link WebGPURenderer} and a WebGPU backend.
 *
 * @three_import import { RadianceCascades, radianceCascadesIrradiance } from 'three/addons/lighting/RadianceCascades.js';
 */
class RadianceCascades {

	/**
	 * Constructs new radiance cascades.
	 *
	 * @param {Object} source - The source of the rays, e.g. a {@link RasterCascadeSource}.
	 * @param {Object} [options] - The options.
	 * @param {number} [options.cascades=3] - The number of cascades.
	 * @param {number} [options.directions=4] - The base directions of the finest cascade along the polar axis, there are
	 * twice as many around it, so 4 gives 32 directions. Every cascade doubles them along both axes.
	 * @param {number} [options.spacing=4] - The probe spacing of the finest cascade in light map texels.
	 * @param {number} [options.maxProbes=65536] - The most probes of the finest cascade, larger light maps get a wider spacing. The
	 * memory of the cascades grows with them, by about 1 KB per probe and cascade, 1.5 KB with `accumulate`.
	 * @param {number} [options.intervalScale=1.6] - The length of the interval of the finest cascade over its probe spacing.
	 * @param {number} [options.lengthScale=4] - The factor of the interval lengths between cascades.
	 * @param {number} [options.depositScale=1] - The size of the texel grid that spawns the rays relative to the light map, `0.5`
	 * spawns them from a quarter of the texels.
	 * @param {boolean} [options.accumulate=false] - Whether the probes keep the rays of all samples, rather than only the rays of
	 * the current sample. Sources whose texels share their directions need it to fill the directions of the probes.
	 * @param {number} [options.nearField=2] - The length of the rays that each texel traces itself, over the probe spacing of the
	 * finest cascade, if the source can trace them ("cascade -1" of the paper). The probes only gather the light beyond, so the
	 * contact shadows of the texels stay sharp. `0` gathers all light from the probes.
	 * @param {boolean} [options.reproject=false] - Whether the rays of the coarser cascades are binned by the direction from the
	 * center of their probe to their hit, rather than by their own direction.
	 */
	constructor( source, { cascades = 3, directions = 4, spacing = 4, maxProbes = 65536, intervalScale = 1.6, lengthScale = 4, depositScale = 1, accumulate = false, nearField = 2, reproject = false } = {} ) {

		/**
		 * The source of the rays.
		 *
		 * @type {Object}
		 */
		this.source = source;

		/**
		 * The number of cascades.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.cascades = cascades;

		/**
		 * The base directions of the finest cascade along the polar axis.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.directions = directions;

		/**
		 * The probe spacing of the finest cascade in light map texels.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.spacing = spacing;

		/**
		 * The most probes of the finest cascade.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.maxProbes = maxProbes;

		/**
		 * The length of the interval of the finest cascade over its probe spacing.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.intervalScale = intervalScale;

		/**
		 * The factor of the interval lengths between cascades.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.lengthScale = lengthScale;

		/**
		 * The size of the texel grid that spawns the rays relative to the light map.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.depositScale = depositScale;

		/**
		 * Whether the probes keep the rays of all samples.
		 *
		 * @type {boolean}
		 * @readonly
		 */
		this.accumulate = accumulate;

		/**
		 * The length of the rays that each texel traces itself, over the probe spacing of the finest cascade.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.nearField = typeof source.nearHit === 'function' || typeof source.nearCorrection === 'function' ? nearField : 0;

		/**
		 * The world-space length of the rays that each texel traces itself, set by `update()`.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.nearDistance = 0;

		/**
		 * Whether the rays of the coarser cascades are binned by the direction from the center of their
		 * probe to their hit, rather than by their own direction.
		 *
		 * @type {boolean}
		 * @default false
		 */
		this.reproject = reproject;

		/**
		 * The world-space probe spacing of the finest cascade, set by `update()`.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.probeSpacing = 0;

		/**
		 * The world-space ends of the intervals of the cascades, the last one is unbounded.
		 *
		 * @type {Array<number>}
		 * @readonly
		 */
		this.intervals = [];

		/**
		 * The number of probes of each cascade, set by `update()`.
		 *
		 * @type {Array<number>}
		 * @readonly
		 */
		this.probes = [];

		/**
		 * The bytes of the storage buffers.
		 *
		 * @type {number}
		 * @readonly
		 */
		this.memory = 0;

		/**
		 * This flag can be used for type testing.
		 *
		 * @type {boolean}
		 * @readonly
		 * @default true
		 */
		this.isRadianceCascades = true;

		this._renderer = null;
		this._depositMapper = null;
		this._kernels = null;
		this._buffers = [];
		this._sampleKey = null;
		this._origin = new Vector3();
		this._bits = [ 0, 0, 0 ];
		this._layout = null;

	}

	/**
	 * Allocates the probes for the meshes of the scene with light map UVs and prepares the passes.
	 * The source must be prepared first. Call this again after changing the scene.
	 *
	 * @param {WebGPURenderer} renderer - The renderer.
	 * @param {Scene} scene - The scene.
	 * @param {number} size - The light map size in texels, which sets the texel density.
	 */
	update( renderer, scene, size ) {

		this._clear();

		this._renderer = renderer;

		const { triangles, groups, meshes, texelSize } = collectTriangles( scene, size );

		this.probeSpacing = Math.max( texelSize * this.spacing, 1e-6 );

		this._allocate( triangles, groups );

		this.nearDistance = this.probeSpacing * this.nearField;
		this.intervals = [];

		for ( let n = 0; n < this.cascades; n ++ ) {

			this.intervals.push( n === this.cascades - 1 ? Infinity : this.probeSpacing * this.intervalScale * this.lengthScale ** n );

		}

		// the rays are spawned from the texels of the light map, rasterized like a bake

		const depositSize = Math.max( Math.round( size * this.depositScale ), 1 );
		const pages = meshes.reduce( ( count, mesh ) => Math.max( count, ( mesh.userData.lightMapPage ?? 0 ) + 1 ), 1 );

		this._depositMapper = new LightMapper( renderer, depositSize, ( position, normal, sample ) => this._deposit( position, normal, sample ), { pages } );
		this._depositMapper.addObjects( meshes );

		this._kernels = this._createKernels();

	}

	/**
	 * Frees all internal resources and the source.
	 */
	dispose() {

		this._clear();
		this.source.dispose?.();

	}

	/**
	 * Frees the probes and passes.
	 *
	 * @private
	 */
	_clear() {

		if ( this._depositMapper !== null ) {

			this._depositMapper.dispose();
			this._depositMapper = null;

		}

		for ( const buffer of this._buffers ) buffer.value.dispose?.();

		this._buffers = [];
		this._kernels = null;
		this._sampleKey = null;

	}

	/**
	 * Builds the probes, their hash tables and neighbors, and the buffers of the cascades.
	 *
	 * @private
	 * @param {Float32Array} triangles - The world-space triangles of the light-mapped meshes.
	 * @param {Uint8Array} groups - The orientations of the triangles, see `collectTriangles()`.
	 */
	_allocate( triangles, groups ) {

		const cascades = this.cascades;

		// the cells of the finest cascade are keyed by their coordinates, the coarser cascades halve them. The
		// spacing grows until the probes fit the budget, their count falls with its square

		const bounds = new Box3();

		for ( let i = 0; i < triangles.length; i += 3 ) bounds.expandByPoint( _a.fromArray( triangles, i ) );

		let spacing, origin, bits, keys;

		for ( let attempt = 0; attempt < 8; attempt ++ ) {

			spacing = this.probeSpacing;
			origin = this._origin.copy( bounds.min ).subScalar( spacing );

			const cells = bounds.getSize( _a ).divideScalar( spacing ).addScalar( 3 ).ceil();
			bits = this._bits = [ cells.x, cells.y, cells.z ].map( ( count ) => Math.max( Math.ceil( Math.log2( count ) ), 1 ) );

			if ( bits[ 0 ] + bits[ 1 ] + bits[ 2 ] > 28 ) {

				this.probeSpacing *= 2;
				continue;

			}

			keys = voxelizeTriangles( triangles, groups, origin, spacing, bits );

			if ( keys.size <= this.maxProbes ) break;

			this.probeSpacing *= Math.sqrt( keys.size / this.maxProbes ) * 1.05;

		}

		// the probes of each cascade, sorted by their keys for coherent merges

		const levels = [];
		let level = Array.from( keys ).sort( ( a, b ) => a - b );

		for ( let n = 0; n < cascades; n ++ ) {

			if ( n > 0 ) level = Array.from( new Set( level.map( ( key ) => shiftKey( key, bits, 1 ) ) ) ).sort( ( a, b ) => a - b );

			levels.push( level );

		}

		this.probes = levels.map( ( probes ) => probes.length );

		// open addressing hash tables of the keys plus one, which leaves zero for empty slots

		const tables = [];
		const tableOffsets = [];
		const tableMasks = [];
		const probeLengths = [];
		let tableSize = 0;

		for ( const probes of levels ) {

			const size = 2 ** Math.ceil( Math.log2( probes.length * 2 + 1 ) );
			const table = new Uint32Array( size * 2 );
			let longest = 0;

			for ( let i = 0; i < probes.length; i ++ ) {

				let slot = mixKey( probes[ i ] + 1 ) & ( size - 1 );
				let length = 0;

				while ( table[ slot * 2 ] !== 0 ) {

					slot = ( slot + 1 ) & ( size - 1 );
					length ++;

				}

				table[ slot * 2 ] = probes[ i ] + 1;
				table[ slot * 2 + 1 ] = i;
				longest = Math.max( longest, length );

			}

			tables.push( table );
			tableOffsets.push( tableSize );
			tableMasks.push( size - 1 );
			probeLengths.push( longest + 1 );
			tableSize += size;

		}

		const table = new Uint32Array( tableSize * 2 );
		tables.forEach( ( part, n ) => table.set( part, tableOffsets[ n ] * 2 ) );

		// the probes of the next cascade around each probe, with their trilinear weights by the parity of the
		// probe: the centers of the coarser cells lie a quarter and three quarters of a cell away

		const keyOffsets = [];
		const neighborOffsets = [];
		let keyCount = 0;

		for ( let n = 0; n < cascades; n ++ ) {

			keyOffsets.push( keyCount );
			neighborOffsets.push( keyCount * 8 );
			keyCount += levels[ n ].length;

		}

		const probeKeys = new Uint32Array( Math.max( keyCount, 2 ) );
		const neighbors = new Uint32Array( Math.max( keyCount * 8, 2 ) ).fill( NONE );

		for ( let n = 0; n < cascades; n ++ ) {

			probeKeys.set( levels[ n ], keyOffsets[ n ] );

			if ( n === cascades - 1 ) continue;

			const next = new Map( levels[ n + 1 ].map( ( key, i ) => [ key, i ] ) );

			for ( let i = 0; i < levels[ n ].length; i ++ ) {

				const [ x, y, z, group ] = decodeKey( levels[ n ][ i ], bits );
				const lower = [ x, y, z ].map( ( c ) => Math.floor( ( c - 1 ) / 2 ) );

				for ( let corner = 0; corner < 8; corner ++ ) {

					const cx = lower[ 0 ] + ( corner & 1 ), cy = lower[ 1 ] + ( ( corner >> 1 ) & 1 ), cz = lower[ 2 ] + ( ( corner >> 2 ) & 1 );

					if ( cx < 0 || cy < 0 || cz < 0 ) continue;

					const index = next.get( encodeKey( cx, cy, cz, bits, group ) );

					if ( index !== undefined ) neighbors[ neighborOffsets[ n ] + i * 8 + corner ] = index;

				}

			}

		}

		// the bins of a probe: its directions, the coarser cascades nest four directions in each one

		const bins = [];
		const depositOffsets = [];
		const mergedOffsets = [];
		let binCount = 0;
		let mergedCount = 0;

		for ( let n = 0; n < cascades; n ++ ) {

			bins.push( this._getDirections( n ) );
			depositOffsets.push( binCount );
			mergedOffsets.push( mergedCount );
			binCount += levels[ n ].length * bins[ n ];
			mergedCount += levels[ n ].length * ( n === 0 ? bins[ 0 ] : bins[ n - 1 ] );

		}

		this._layout = { tableOffsets, tableMasks, probeLengths, keyOffsets, neighborOffsets, bins, depositOffsets, mergedOffsets, binCount, mergedCount };

		const depositData = new Uint32Array( Math.max( binCount * STRIDE, 2 ) );

		const tableNode = storage( new StorageBufferAttribute( table, 2 ), 'uvec2', tableSize ).toReadOnly();
		const keysNode = storage( new StorageBufferAttribute( probeKeys, 1 ), 'uint', probeKeys.length ).toReadOnly();
		const neighborsNode = storage( new StorageBufferAttribute( neighbors, 1 ), 'uint', neighbors.length ).toReadOnly();
		const depositNode = storage( new StorageBufferAttribute( depositData, 1 ), 'uint', depositData.length ).toAtomic();
		const mergedNode = storage( new StorageBufferAttribute( new Float32Array( Math.max( mergedCount, 2 ) * 4 ), 4 ), 'vec4', Math.max( mergedCount, 2 ) );
		const mergedDirectionsNode = storage( new StorageBufferAttribute( new Float32Array( Math.max( mergedCount, 2 ) * 4 ), 4 ), 'vec4', Math.max( mergedCount, 2 ) );

		this._table = tableNode;
		this._keys = keysNode;
		this._neighbors = neighborsNode;
		this._deposits = depositNode;
		this._merged = mergedNode;
		this._mergedDirections = mergedDirectionsNode;
		this._buffers = [ tableNode, keysNode, neighborsNode, depositNode, mergedNode, mergedDirectionsNode ];

		if ( this.accumulate ) {

			this._sums = storage( new StorageBufferAttribute( new Float32Array( Math.max( binCount, 2 ) * 4 ), 4 ), 'vec4', Math.max( binCount, 2 ) );
			this._counts = storage( new StorageBufferAttribute( new Float32Array( Math.max( binCount, 2 ) ), 1 ), 'float', Math.max( binCount, 2 ) );
			this._buffers.push( this._sums, this._counts );

		}

		// the cosine-weighted sub-directions of the base directions, after their centers

		const base = bins[ 0 ];
		const theta = this.directions;
		const directionData = new Float32Array( ( base + base * COSINE_SAMPLES * COSINE_SAMPLES ) * 4 );

		for ( let d = 0; d < base; d ++ ) {

			const i = d % ( 2 * theta ), j = Math.floor( d / ( 2 * theta ) );

			decodeDirection( ( i + 0.5 ) / ( 2 * theta ), ( j + 0.5 ) / theta, _a ).toArray( directionData, d * 4 );

			for ( let s = 0; s < COSINE_SAMPLES * COSINE_SAMPLES; s ++ ) {

				const u = ( i + ( s % COSINE_SAMPLES + 0.5 ) / COSINE_SAMPLES ) / ( 2 * theta );
				const v = ( j + ( Math.floor( s / COSINE_SAMPLES ) + 0.5 ) / COSINE_SAMPLES ) / theta;

				decodeDirection( u, v, _a ).toArray( directionData, ( base + d * COSINE_SAMPLES * COSINE_SAMPLES + s ) * 4 );

			}

		}

		this._directionData = storage( new StorageBufferAttribute( directionData, 4 ), 'vec4', directionData.length / 4 ).toReadOnly();
		this._buffers.push( this._directionData );

		this.memory = this._buffers.reduce( ( bytes, buffer ) => bytes + buffer.value.array.byteLength, 0 );

	}

	/**
	 * Returns the number of directions of the probes of a cascade.
	 *
	 * @private
	 * @param {number} n - The cascade.
	 * @return {number} The directions.
	 */
	_getDirections( n ) {

		const theta = this.directions * 2 ** n;

		return 2 * theta * theta;

	}

	/**
	 * Starts over with the next sample of the light mapper, call it after resetting the light mapper. The
	 * rays of a sample are added once for all the bake materials and pages that render it, by the index
	 * of the sample, which repeats after a reset.
	 */
	restart() {

		this._sampleKey = null;

	}

	/**
	 * Adds the rays of a sample and merges the cascades, once per sample of a light mapper, which its
	 * bake materials and pages share.
	 *
	 * @private
	 * @param {WebGPURenderer} renderer - The renderer.
	 * @param {Node<float>} sampleNode - The sample index of the light mapper.
	 */
	_update( renderer, sampleNode ) {

		const sampleKey = `${ sampleNode.id },${ sampleNode.value }`;

		if ( sampleKey === this._sampleKey || this._kernels === null ) return;

		this._sampleKey = sampleKey;

		const kernels = this._kernels;

		// a new bake discards the rays of the previous one

		if ( this.accumulate && sampleNode.value === 0 ) renderer.compute( kernels.clearSums );

		this.source.updateSample?.( renderer );

		this._depositMapper.update();

		if ( this.accumulate ) renderer.compute( kernels.flush );

		for ( let n = this.cascades - 1; n >= 0; n -- ) renderer.compute( kernels.merge[ n ] );

		if ( this.accumulate === false ) renderer.compute( kernels.clear );

	}

	/**
	 * Returns the probe of each cascade at a world-space position, or `NONE` where there is none. The probes
	 * are shared by the texels of the same orientation, so that the hidden or opposite sides of nearby
	 * surfaces, e.g. of a thin wall, do not mix their light.
	 *
	 * @param {Node<vec3>} position - The world-space position.
	 * @param {Node<vec3>} faceNormal - The face normal.
	 * @return {Array<Node<uint>>} The probes.
	 */
	probesAt( position, faceNormal ) {

		const cell = this._cellOf( position, 0 );
		const group = groupOf( faceNormal );

		return Array.from( { length: this.cascades }, ( value, n ) => this._lookup( this._keyOf( n === 0 ? cell : cell.shiftRight( uvec3( n ) ), group ), n ).toVar() );

	}

	/**
	 * Adds a ray that hits a surface at a distance to the cascades: its radiance to the cascade whose
	 * interval holds the distance, and full transmittance to the cascades below. A ray that leaves the
	 * scene adds its radiance to the last cascade.
	 *
	 * @param {Array<Node<uint>>} probes - The probes of the origin of the ray, see `probesAt()`.
	 * @param {Node<vec3>} direction - The world-space direction of the ray.
	 * @param {Node<float>} distance - The distance of the hit, negative if the ray leaves the scene.
	 * @param {Node<vec3>} radiance - The radiance of the hit, or that the ray sees when it leaves the scene.
	 * @param {Node<vec3>} origin - The origin of the ray.
	 */
	depositHit( probes, direction, distance, radiance, origin ) {

		const bins = this._binsOf( direction );
		const hitCascade = this._cascadeOf( distance ).toVar();

		for ( let n = 0; n < this.cascades; n ++ ) {

			// the probes interpolate their light from their centers, so the rays of the coarser cascades are
			// binned by the direction from the center to their hit, or to the end of their interval. Otherwise
			// the directions of distant lights shift with the texels that spawn the rays

			const end = n === this.cascades - 1 ? null : float( this.intervals[ n ] );
			const reprojected = ( point ) => n === 0 || this.reproject === false ? bins : this._binsOf( normalize( point.sub( this._centerOf( origin, n ) ) ) );

			const hit = If( hitCascade.equal( n ), () => {

				const hitBins = end === null ? bins : reprojected( origin.add( direction.mul( distance ) ) );

				this._depositBin( probes[ n ], n, hitBins, radiance, float( 0 ) );

			} );

			// the last cascade is unbounded, rays pass through all others

			if ( end !== null ) {

				hit.ElseIf( hitCascade.greaterThan( n ), () => {

					this._depositBin( probes[ n ], n, reprojected( origin.add( direction.mul( end ) ) ), null, float( 1 ) );

				} );

			}

		}

	}

	/**
	 * Returns the center of the cell of a world-space position in a cascade.
	 *
	 * @private
	 * @param {Node<vec3>} position - The position.
	 * @param {number} n - The cascade.
	 * @return {Node<vec3>} The center.
	 */
	_centerOf( position, n ) {

		const size = this.probeSpacing * 2 ** n;

		return floor( position.sub( this._origin ).div( size ) ).add( 0.5 ).mul( size ).add( this._origin );

	}

	/**
	 * Adds the radiance and transmittance that a ray gathers in the interval of one cascade.
	 *
	 * @param {Array<Node<uint>>} probes - The probes of the origin of the ray, see `probesAt()`.
	 * @param {number} n - The cascade.
	 * @param {Node<vec3>} direction - The world-space direction of the ray.
	 * @param {Node<vec3>} radiance - The radiance gathered in the interval, premultiplied by its opacity.
	 * @param {Node<float>} transmittance - The transmittance of the interval.
	 */
	depositInterval( probes, n, direction, radiance, transmittance ) {

		this._depositBin( probes[ n ], n, this._binsOf( direction ), radiance, transmittance );

	}

	/**
	 * Returns the index of the cascade whose interval holds a distance, the last one for negative distances.
	 *
	 * @private
	 * @param {Node<float>} distance - The distance.
	 * @return {Node<int>} The cascade.
	 */
	_cascadeOf( distance ) {

		const cascade = int( this.cascades - 1 ).toVar();

		for ( let n = this.cascades - 2; n >= 0; n -- ) {

			If( distance.greaterThanEqual( 0 ).and( distance.lessThan( this.intervals[ n ] ) ), () => {

				cascade.assign( n );

			} );

		}

		// the texels trace the hits of the near field themselves, the probes only see beyond it

		if ( this.nearDistance > 0 ) {

			If( distance.greaterThanEqual( 0 ).and( distance.lessThan( this.nearDistance ) ), () => {

				cascade.assign( - 1 );

			} );

		}

		return cascade;

	}

	/**
	 * Returns the equal-area coordinates of a direction, see "Algorithm 2" of the paper: the azimuth and
	 * the height along z, both in [0, 1).
	 *
	 * @private
	 * @param {Node<vec3>} direction - The unit direction.
	 * @return {Node<vec2>} The coordinates.
	 */
	_binsOf( direction ) {

		return vec2( fract( atan( direction.y, direction.x ).div( 2 * Math.PI ) ), direction.z.mul( 0.5 ).add( 0.5 ).clamp( 0, 0.9999999 ) ).toVar();

	}

	/**
	 * Returns the bin of equal-area coordinates in a cascade.
	 *
	 * @private
	 * @param {Node<vec2>} coords - The coordinates, see `_binsOf()`.
	 * @param {number} n - The cascade.
	 * @return {Node<uint>} The bin.
	 */
	_bin( coords, n ) {

		const theta = this.directions * 2 ** n;
		const i = coords.x.mul( 2 * theta ).toUint().min( 2 * theta - 1 );
		const j = coords.y.mul( theta ).toUint().min( theta - 1 );

		return j.mul( 2 * theta ).add( i );

	}

	/**
	 * Adds a ray to a bin of a probe.
	 *
	 * @private
	 */
	_depositBin( probe, n, coords, radiance, transmittance ) {

		const layout = this._layout;
		const deposits = this._deposits;

		If( probe.notEqual( NONE ), () => {

			const base = probe.mul( layout.bins[ n ] ).add( this._bin( coords, n ) ).add( layout.depositOffsets[ n ] ).mul( STRIDE ).toVar();

			if ( radiance !== null ) {

				const value = radiance.max( 0 ).mul( SCALE ).toVar();

				for ( let channel = 0; channel < 3; channel ++ ) {

					const component = value[ 'xyz'[ channel ] ];

					// the low word carries into the high word when it wraps around

					If( component.greaterThanEqual( 1 ), () => {

						const high = floor( component.mul( 2 ** - 32 ) ).toVar();
						const low = uint( component.sub( high.mul( 2 ** 32 ) ) ).toVar();
						const previous = atomicAdd( deposits.element( base.add( channel * 2 ) ), low ).toVar();
						const carry = uint( high ).add( select( previous.greaterThan( uint( 0xffffffff ).sub( low ) ), uint( 1 ), uint( 0 ) ) ).toVar();

						If( carry.greaterThan( 0 ), () => {

							atomicAdd( deposits.element( base.add( channel * 2 + 1 ) ), carry );

						} );

					} );

				}

			}

			const beta = uint( transmittance.clamp( 0, 1 ).mul( BETA_SCALE ).add( 0.5 ) ).toVar();

			If( beta.greaterThan( 0 ), () => {

				atomicAdd( deposits.element( base.add( 6 ) ), beta );

			} );

			atomicAdd( deposits.element( base.add( 7 ) ), uint( 1 ) );

		} );

	}

	/**
	 * Returns the cell of a world-space position in a cascade.
	 *
	 * @private
	 * @param {Node<vec3>} position - The position.
	 * @param {number} n - The cascade.
	 * @return {Node<uvec3>} The cell.
	 */
	_cellOf( position, n ) {

		const cell = floor( position.sub( this._origin ).div( this.probeSpacing * 2 ** n ) ).max( 0 );

		return uvec3( cell ).toVar();

	}

	/**
	 * Returns the hash key of a cell, plus one.
	 *
	 * @private
	 * @param {Node<uvec3>} cell - The cell.
	 * @param {Node<uint>} group - The orientation, see `groupOf()`.
	 * @return {Node<uint>} The key.
	 */
	_keyOf( cell, group ) {

		const [ bx, by, bz ] = this._bits;

		return cell.x.bitOr( cell.y.shiftLeft( uint( bx ) ) ).bitOr( cell.z.shiftLeft( uint( bx + by ) ) ).bitOr( group.shiftLeft( uint( bx + by + bz ) ) ).add( 1 );

	}

	/**
	 * Returns the probe of a key in the hash table of a cascade, or `NONE`.
	 *
	 * @private
	 * @param {Node<uint>} key - The key plus one, see `_keyOf()`.
	 * @param {number} n - The cascade.
	 * @return {Node<uint>} The probe.
	 */
	_lookup( key, n ) {

		const { tableOffsets, tableMasks, probeLengths } = this._layout;
		const table = this._table;

		return Fn( () => {

			const slot = mixKeyNode( key ).bitAnd( tableMasks[ n ] ).toVar();
			const probe = uint( NONE ).toVar();

			Loop( probeLengths[ n ], () => {

				const entry = table.element( slot.add( tableOffsets[ n ] ) ).toVar();

				If( entry.x.equal( key ), () => {

					probe.assign( entry.y );
					Break();

				} );

				If( entry.x.equal( 0 ), () => {

					Break();

				} );

				slot.assign( slot.add( 1 ).bitAnd( tableMasks[ n ] ) );

			} );

			return probe;

		} )();

	}

	/**
	 * The fragment shader of the deposit pass: the source traces the rays of a texel.
	 *
	 * @private
	 */
	_deposit( position, normal, sample ) {

		return Fn( () => {

			const point = position.toVar();

			// a degenerate normal would poison the cascades with NaNs

			const normalLength = length( normal ).toVar();
			const unitNormal = select( normalLength.greaterThan( 1e-8 ), normal.div( normalLength ), vec3( 0, 1, 0 ) ).toVar();

			// rays must leave the receiving face, otherwise they would hit the face itself

			const face = cross( dFdx( point ), dFdy( point ) ).toVar();
			const faceLength = length( face ).toVar();
			const faceNormal = select( faceLength.greaterThan( 1e-12 ), face.div( faceLength ), unitNormal ).toVar();
			faceNormal.assign( select( dot( faceNormal, unitNormal ).lessThan( 0 ), faceNormal.negate(), faceNormal ) );

			this.source.trace( this, point, unitNormal, faceNormal, sample );

			return vec3( 0 );

		} )();

	}

	/**
	 * Returns uniformly distributed directions in the hemisphere of a face, from the equal-area mapping
	 * of a low-discrepancy sequence that continues over the samples. Directions below the face are flipped.
	 *
	 * @param {Node<vec3>} faceNormal - The face normal.
	 * @param {Node<uint>} index - The index of the direction in the sequence.
	 * @param {Node<vec2>} offset - A random offset of the sequence.
	 * @return {Node<vec3>} The direction.
	 */
	hemisphereDirection( faceNormal, index, offset ) {

		const u = fract( offset.add( vec2( float( index.mul( R2[ 0 ] ) ), float( index.mul( R2[ 1 ] ) ) ).mul( 2 ** - 32 ) ) ).toVar();
		const z = u.y.mul( 2 ).sub( 1 );
		const r = sqrt( z.mul( z ).oneMinus().max( 0 ) );
		const phi = u.x.mul( 2 * Math.PI );
		const direction = vec3( r.mul( cos( phi ) ), r.mul( sin( phi ) ), z ).toVar();

		return select( dot( direction, faceNormal ).lessThan( 0 ), direction.negate(), direction );

	}

	/**
	 * Creates the passes that clear, flush and merge the cascades.
	 *
	 * @private
	 * @return {Object} The passes.
	 */
	_createKernels() {

		const layout = this._layout;
		const deposits = this._deposits;
		const sums = this._sums;
		const counts = this._counts;
		const binCount = layout.binCount;
		const accumulate = this.accumulate;

		const clear = Fn( () => {

			If( instanceIndex.lessThan( binCount * STRIDE ), () => {

				atomicStore( deposits.element( instanceIndex ), uint( 0 ) );

			} );

		} )().compute( Math.max( binCount * STRIDE, 1 ) ).setName( 'RadianceCascades.Clear' );

		let flush = null, clearSums = null;

		if ( accumulate ) {

			// the deposits of a sample are added to the float sums of all samples

			flush = Fn( () => {

				If( instanceIndex.lessThan( binCount ), () => {

					const base = instanceIndex.mul( STRIDE ).toVar();
					const rays = atomicLoad( deposits.element( base.add( 7 ) ) ).toVar();

					If( rays.notEqual( 0 ), () => {

						const radiance = radianceOf( base );
						const beta = float( atomicLoad( deposits.element( base.add( 6 ) ) ) ).div( BETA_SCALE );

						sums.element( instanceIndex ).addAssign( vec4( radiance, beta ) );
						counts.element( instanceIndex ).addAssign( float( rays ) );

						for ( let k = 0; k < STRIDE; k ++ ) atomicStore( deposits.element( base.add( k ) ), uint( 0 ) );

					} );

				} );

			} )().compute( Math.max( binCount, 1 ) ).setName( 'RadianceCascades.Flush' );

			clearSums = Fn( () => {

				If( instanceIndex.lessThan( binCount ), () => {

					sums.element( instanceIndex ).assign( vec4( 0 ) );
					counts.element( instanceIndex ).assign( 0 );

				} );

			} )().compute( Math.max( binCount, 1 ) ).setName( 'RadianceCascades.ClearSums' );

		}

		// the sum of the radiance of a bin from its words

		const radianceOf = ( base ) => {

			const word = ( k ) => float( atomicLoad( deposits.element( base.add( k ) ) ) );

			return vec3( word( 1 ), word( 3 ), word( 5 ) ).mul( 2 ** 32 ).add( vec3( word( 0 ), word( 2 ), word( 4 ) ) ).div( SCALE );

		};

		// the mean radiance, transmittance and ray count of a bin

		const readBin = ( bin ) => {

			const result = vec4( 0 ).toVar();
			const count = float( 0 ).toVar();

			if ( accumulate ) {

				count.assign( counts.element( bin ) );

				If( count.greaterThan( 0 ), () => {

					result.assign( sums.element( bin ).div( count ) );

				} );

			} else {

				const base = bin.mul( STRIDE ).toVar();

				count.assign( float( atomicLoad( deposits.element( base.add( 7 ) ) ) ) );

				If( count.greaterThan( 0 ), () => {

					const radiance = radianceOf( base );
					const beta = float( atomicLoad( deposits.element( base.add( 6 ) ) ) ).div( BETA_SCALE );

					result.assign( vec4( radiance, beta ).div( count ) );

				} );

			}

			return { mean: result, count };

		};

		const merge = [];

		for ( let n = 0; n < this.cascades; n ++ ) merge.push( this._createMergeKernel( n, readBin ) );

		return { clear, flush, clearSums, merge };

	}

	/**
	 * Creates the pass that merges a cascade with the next coarser one. The finest cascade stores the
	 * radiance of its directions, the others the mean of the four directions within each direction of
	 * the next finer cascade ("pre-averaging"), since only that one reads them.
	 *
	 * @private
	 * @param {number} n - The cascade.
	 * @param {function} readBin - Returns the mean and count of a bin.
	 * @return {ComputeNode} The pass.
	 */
	_createMergeKernel( n, readBin ) {

		const layout = this._layout;
		const merged = this._merged;
		const mergedDirections = this._mergedDirections;
		const keys = this._keys;
		const neighbors = this._neighbors;
		const probes = this.probes[ n ];
		const bins = layout.bins[ n ];
		const outputs = n === 0 ? bins : layout.bins[ n - 1 ];
		const count = probes * outputs;
		const last = n === this.cascades - 1;
		const theta = this.directions * 2 ** n;
		const [ bx, by ] = this._bits;

		return Fn( () => {

			If( instanceIndex.lessThan( count ), () => {

				const probe = instanceIndex.div( outputs ).toVar();
				const output = instanceIndex.mod( outputs ).toVar();

				// the trilinear weights of the probes of the next cascade around this one, by its parity

				const corners = [];

				if ( last === false ) {

					const key = keys.element( probe.add( layout.keyOffsets[ n ] ) ).toVar();
					const parity = uvec3( key, key.shiftRight( uint( bx ) ), key.shiftRight( uint( bx + by ) ) ).bitAnd( uvec3( 1 ) ).toVar();
					const lowerWeight = select( parity.equal( uvec3( 0 ) ), vec3( 0.25 ), vec3( 0.75 ) ).toVar();

					for ( let corner = 0; corner < 8; corner ++ ) {

						const index = neighbors.element( probe.mul( 8 ).add( layout.neighborOffsets[ n ] + corner ) ).toVar();
						const wx = ( corner & 1 ) ? lowerWeight.x.oneMinus() : lowerWeight.x;
						const wy = ( corner & 2 ) ? lowerWeight.y.oneMinus() : lowerWeight.y;
						const wz = ( corner & 4 ) ? lowerWeight.z.oneMinus() : lowerWeight.z;

						corners.push( { index, weight: wx.mul( wy ).mul( wz ).toVar() } );

					}

				}

				// the radiance beyond the interval in a direction of this cascade, interpolated from the next one,
				// with the sum of the directions of its parts weighted by their luminance

				const beyond = ( bin ) => {

					const sum = vec4( 0 ).toVar();
					const directionSum = vec3( 0 ).toVar();

					for ( const { index, weight } of corners ) {

						If( index.notEqual( NONE ), () => {

							const item = index.mul( bins ).add( bin ).add( layout.mergedOffsets[ n + 1 ] ).toVar();
							const value = merged.element( item ).toVar();
							const w = value.w.mul( weight ).toVar();

							sum.addAssign( vec4( value.rgb, 1 ).mul( w ) );
							directionSum.addAssign( mergedDirections.element( item ).xyz.mul( w ) );

						} );

					}

					const inverse = float( 1 ).div( sum.w.max( 1e-8 ) );

					return { radiance: sum.rgb.mul( inverse ), directions: directionSum.mul( inverse ) };

				};

				// the light of a direction of this cascade: its interval, then the light beyond if it is transparent

				const lightOf = ( bin ) => {

					const { mean, count: rays } = readBin( probe.mul( bins ).add( bin ).add( layout.depositOffsets[ n ] ) );
					const radiance = mean.rgb.toVar();
					const directions = binDirection( bin, theta ).mul( luminance( mean.rgb ) ).toVar();

					if ( last === false ) {

						const far = beyond( bin );

						radiance.addAssign( far.radiance.mul( mean.a ) );
						directions.addAssign( far.directions.mul( mean.a ) );

					}

					return { radiance, directions, valid: rays.greaterThan( 0 ) };

				};

				const item = instanceIndex.add( layout.mergedOffsets[ n ] );

				if ( n === 0 ) {

					const { radiance, directions, valid } = lightOf( output );

					merged.element( item ).assign( select( valid, vec4( radiance, 1 ), vec4( 0 ) ) );
					mergedDirections.element( item ).assign( select( valid, vec4( directions, 0 ), vec4( 0 ) ) );

				} else {

					// the four directions of this cascade within a direction of the finer one

					const parentTheta = theta / 2;
					const column = output.mod( 2 * parentTheta ).mul( 2 ).toVar();
					const row = output.div( 2 * parentTheta ).mul( 2 ).toVar();
					const sum = vec4( 0 ).toVar();
					const directionSum = vec3( 0 ).toVar();

					for ( let child = 0; child < 4; child ++ ) {

						const bin = row.add( child >> 1 ).mul( 2 * theta ).add( column.add( child & 1 ) );
						const { radiance, directions, valid } = lightOf( bin );

						If( valid, () => {

							sum.addAssign( vec4( radiance, 1 ) );
							directionSum.addAssign( directions );

						} );

					}

					const inverse = float( 1 ).div( sum.w.max( 1 ) );

					merged.element( item ).assign( select( sum.w.greaterThan( 0 ), vec4( sum.rgb.mul( inverse ), 1 ), vec4( 0 ) ) );
					mergedDirections.element( item ).assign( vec4( directionSum.mul( inverse ), 0 ) );

				}

			} );

		} )().compute( Math.max( count, 1 ) ).setName( 'RadianceCascades.Merge' + n );

	}


	/**
	 * Returns the irradiance at a world-space position and normal, gathered from the interpolated probes
	 * of the finest cascade.
	 *
	 * @private
	 * @param {Node<vec3>} positionNode - The world-space position.
	 * @param {Node<vec3>} normalNode - The world-space normal.
	 * @param {?Node<vec3>} [directionNode=null] - A variable to add the luminance-weighted light directions to.
	 * @return {Node<vec3>} The irradiance.
	 */
	_shade( positionNode, normalNode, sampleNode, directionNode = null ) {

		const layout = this._layout;
		const merged = this._merged;
		const mergedDirections = this._mergedDirections;
		const directionData = this._directionData;
		const base = layout.bins[ 0 ];
		const samples = COSINE_SAMPLES * COSINE_SAMPLES;
		const solidAngle = 4 * Math.PI / base / samples;
		const near = this.nearDistance > 0 && typeof this.source.nearHit === 'function';
		const nearCorrection = this.nearDistance > 0 && typeof this.source.nearCorrection === 'function';
		const theta = this.directions;

		return Fn( () => {

			const position = positionNode.toVar();
			const normalLength = length( normalNode ).toVar();
			const normal = select( normalLength.greaterThan( 1e-8 ), normalNode.div( normalLength ), vec3( 0, 1, 0 ) ).toVar();

			// the face normal selects the probes, and the near rays must leave the face, see _deposit()

			const face = cross( dFdx( position ), dFdy( position ) ).toVar();
			const faceLength = length( face ).toVar();
			const faceNormal = select( faceLength.greaterThan( 1e-12 ), face.div( faceLength ), normal ).toVar();
			faceNormal.assign( select( dot( faceNormal, normal ).lessThan( 0 ), faceNormal.negate(), faceNormal ) );

			const group = groupOf( faceNormal ).toVar();

			let jitter = null;

			if ( near ) {

				// a random position of the near ray within each direction, per texel and sample

				const pixel = uvec2( screenCoordinate.xy );
				const offset = vec2( hash( pixel.x.add( pixel.y.mul( 65536 ) ) ), hash( pixel.y.add( pixel.x.mul( 65536 ) ).add( 7 ) ) );
				const index = uint( sampleNode );

				jitter = fract( offset.add( vec2( float( index.mul( R2[ 0 ] ) ), float( index.mul( R2[ 1 ] ) ) ).mul( 2 ** - 32 ) ) ).toVar();

			}

			// the probes around the position and their trilinear weights

			const grid = position.sub( this._origin ).div( this.probeSpacing ).sub( 0.5 ).toVar();
			const lower = floor( grid ).toVar();
			const t = grid.sub( lower ).toVar();
			const corners = [];

			for ( let corner = 0; corner < 8; corner ++ ) {

				const offset = vec3( corner & 1, ( corner >> 1 ) & 1, ( corner >> 2 ) & 1 );
				const cell = lower.add( offset ).toVar();
				const inside = cell.x.greaterThanEqual( 0 ).and( cell.y.greaterThanEqual( 0 ) ).and( cell.z.greaterThanEqual( 0 ) );
				const index = select( inside, this._lookup( this._keyOf( uvec3( cell.max( 0 ) ), group ), 0 ), uint( NONE ) ).toVar();
				const w = vec3( corner & 1 ? t.x : t.x.oneMinus(), corner & 2 ? t.y : t.y.oneMinus(), corner & 4 ? t.z : t.z.oneMinus() );

				corners.push( { index, weight: w.x.mul( w.y ).mul( w.z ).toVar() } );

			}

			const irradiance = vec3( 0 ).toVar();
			const lightDirection = directionNode !== null ? vec3( 0 ).toVar() : null;

			Loop( { start: 0, end: base, type: 'uint', condition: '<', name: 'bin' }, ( { bin } ) => {

				// the cosine of the receiver integrated over the direction

				const cosine = float( 0 ).toVar();
				const horizon = float( 1 ).toVar();

				Loop( { start: 0, end: samples, type: 'uint', condition: '<', name: 's' }, ( { s } ) => {

					const sampleCosine = dot( directionData.element( bin.mul( samples ).add( s ).add( base ) ).xyz, normal ).toVar();

					cosine.addAssign( sampleCosine.max( 0 ) );
					horizon.assign( min( horizon, sampleCosine ) );

				} );

				If( cosine.greaterThan( 0 ), () => {

					const sum = vec4( 0 ).toVar();
					const directionSum = vec3( 0 ).toVar();

					for ( const { index, weight } of corners ) {

						If( index.notEqual( NONE ), () => {

							const item = index.mul( base ).add( bin ).add( layout.mergedOffsets[ 0 ] ).toVar();
							const value = merged.element( item ).toVar();
							const w = value.w.mul( weight ).toVar();

							sum.addAssign( vec4( value.rgb, 1 ).mul( w ) );
							directionSum.addAssign( mergedDirections.element( item ).xyz.mul( w ) );

						} );

					}

					const radiance = sum.rgb.div( sum.w.max( 1e-8 ) ).toVar();
					const directions = directionSum.div( sum.w.max( 1e-8 ) ).toVar();

					// the light of a direction that lies above the horizon arrives from the luminance-weighted mean of
					// its directions, which keeps small bright lights within it exact. Directions across the horizon
					// assume uniform light

					const weight = cosine.mul( solidAngle ).toVar();

					If( horizon.greaterThan( 0 ).and( luminance( radiance ).greaterThan( 1e-12 ) ), () => {

						weight.assign( dot( directions, normal ).div( luminance( radiance ) ).max( 0 ).mul( 4 * Math.PI / base ) );

					} );

					const light = radiance.mul( weight ).toVar();

					// the near ray sees the surfaces closer than the probes, and the probes beyond them

					if ( near ) {

						const coords = vec2( float( bin.mod( 2 * theta ) ), float( bin.div( 2 * theta ) ) ).add( jitter ).div( vec2( 2 * theta, theta ) );
						const z = coords.y.mul( 2 ).sub( 1 ).toVar();
						const r = sqrt( z.mul( z ).oneMinus().max( 0 ) );
						const phi = coords.x.mul( 2 * Math.PI );
						const nearDirection = vec3( r.mul( cos( phi ) ), r.mul( sin( phi ) ), z ).toVar();

						If( dot( nearDirection, faceNormal ).greaterThan( 0 ), () => {

							const { radiance: nearRadiance, transmittance } = this.source.nearHit( position, faceNormal, nearDirection, float( this.nearDistance ) );

							light.assign( nearRadiance.mul( dot( nearDirection, normal ).max( 0 ).mul( 4 * Math.PI / base ) ).add( light.mul( transmittance ) ) );

						} );

					}

					irradiance.addAssign( light );

					if ( lightDirection !== null ) lightDirection.addAssign( select( dot( directions, directions ).greaterThan( 0 ), normalize( directions ), directionData.element( bin ).xyz ).mul( luminance( light ) ) );

				} );

			} );

			// sources that cannot trace the near rays of each direction correct the light of the probes along
			// their own rays that hit closer than the probes see

			if ( nearCorrection ) {

				const farRadiance = ( direction ) => {

					const bin = this._bin( this._binsOf( direction ), 0 ).toVar();
					const sum = vec4( 0 ).toVar();

					for ( const { index, weight } of corners ) {

						If( index.notEqual( NONE ), () => {

							const value = merged.element( index.mul( base ).add( bin ).add( layout.mergedOffsets[ 0 ] ) ).toVar();

							sum.addAssign( vec4( value.rgb, 1 ).mul( value.w.mul( weight ) ) );

						} );

					}

					return sum.rgb.div( sum.w.max( 1e-8 ) );

				};

				irradiance.addAssign( this.source.nearCorrection( { position, normal, faceNormal, farRadiance, maxDistance: this.nearDistance, directionNode: lightDirection } ) );

			}

			if ( directionNode !== null ) directionNode.addAssign( lightDirection );

			return irradiance;

		} )();

	}

}

/**
 * Gathers the irradiance of {@link RadianceCascades} and adds the rays of a new sample to them once per
 * sample of a light mapper.
 *
 * @private
 * @augments Node
 */
class RadianceCascadesIrradianceNode extends Node {

	static get type() {

		return 'RadianceCascadesIrradianceNode';

	}

	/**
	 * Constructs a new radiance cascades irradiance node.
	 *
	 * @param {RadianceCascades} cascades - The radiance cascades.
	 * @param {Node<vec3>} positionNode - The world-space position.
	 * @param {Node<vec3>} normalNode - The world-space normal.
	 * @param {Node<float>} sampleNode - The sample index of the light mapper.
	 * @param {?Node<vec3>} [directionNode=null] - A variable to add the luminance-weighted light directions to.
	 */
	constructor( cascades, positionNode, normalNode, sampleNode, directionNode = null ) {

		super( 'vec3' );

		this.cascades = cascades;
		this.positionNode = positionNode;
		this.normalNode = normalNode;
		this.sampleNode = sampleNode;
		this.directionNode = directionNode;

		this.updateBeforeType = NodeUpdateType.RENDER;

	}

	updateBefore( frame ) {

		this.cascades._update( frame.renderer, this.sampleNode );

	}

	setup() {

		const cascades = this.cascades;
		const irradiance = cascades._shade( this.positionNode, this.normalNode, this.sampleNode, this.directionNode );
		const direct = cascades.source.direct?.( this.positionNode, this.normalNode, this.directionNode ) ?? null;

		return direct !== null ? irradiance.add( direct ) : irradiance;

	}

}

/**
 * Creates a world-space diffuse irradiance sampler for prepared {@link RadianceCascades}. Every sample of
 * {@link LightMapper} adds the rays of a new sample to the cascades. The sampler adds the
 * luminance-weighted light directions to its optional fourth argument, for the directional light maps
 * of {@link LightMapper}.
 *
 * @param {RadianceCascades} cascades - The prepared radiance cascades.
 * @return {function(Node<vec3>, Node<vec3>, Node<float>, ?Node<vec3>): Node<vec3>} The irradiance sampler.
 */
function radianceCascadesIrradiance( cascades ) {

	return ( position, normal, sample, direction = null ) => nodeObject( new RadianceCascadesIrradianceNode( cascades, position, normal, sample, direction ) );

}

/**
 * A ray source of {@link RadianceCascades} that uses the illumination map of a {@link StochasticRadiosity}
 * solver: every sample rasterizes the scene along a set of directions, and each texel takes the nearest
 * surface in front of it along each one. All texels share the directions of a sample, so the probes
 * need to accumulate the samples, see the `accumulate` option of {@link RadianceCascades}.
 */
class RasterCascadeSource {

	/**
	 * Constructs a new raster ray source.
	 *
	 * @param {StochasticRadiosity} radiosity - The prepared radiosity solver.
	 */
	constructor( radiosity ) {

		/**
		 * The radiosity solver.
		 *
		 * @type {StochasticRadiosity}
		 */
		this.radiosity = radiosity;

	}

	/**
	 * Rotates the directions of the radiosity solver and renders its illumination map, before the rays
	 * of a sample are deposited.
	 *
	 * @param {WebGPURenderer} renderer - The renderer.
	 */
	updateSample( renderer ) {

		this.radiosity._renderPass( renderer );

	}

	/**
	 * Frees the radiosity solver.
	 */
	dispose() {

		this.radiosity.dispose();

	}

	/**
	 * Returns the change of the irradiance of a texel by the surfaces closer than the probes see, along the
	 * directions of the current sample: the light of these surfaces replaces the light of the probes.
	 *
	 * @param {Object} parameters - The texel.
	 * @param {Node<vec3>} parameters.position - The world-space position.
	 * @param {Node<vec3>} parameters.normal - The world-space normal.
	 * @param {Node<vec3>} parameters.faceNormal - The face normal.
	 * @param {function(Node<vec3>): Node<vec3>} parameters.farRadiance - The radiance of the probes in a direction.
	 * @param {number} parameters.maxDistance - The distance that the probes see beyond.
	 * @param {?Node<vec3>} parameters.directionNode - A variable to add the luminance-weighted light directions to.
	 * @return {Node<vec3>} The change of the irradiance.
	 */
	nearCorrection( { position, normal, faceNormal, farRadiance, maxDistance, directionNode } ) {

		const radiosity = this.radiosity;
		const nearest = radiosity._createNearest( position, faceNormal );
		const correction = vec3( 0 ).toVar();

		Loop( radiosity.directions, ( { i } ) => {

			const direction = radiosity._directionNode.element( i ).toVar();
			const cosine = dot( normal, direction ).toVar();

			If( cosine.greaterThan( 0 ).and( dot( direction, faceNormal ).greaterThan( 0 ) ), () => {

				const { distance, radiance } = nearest( i );

				If( distance.greaterThanEqual( 0 ).and( distance.lessThan( maxDistance ) ), () => {

					const change = radiance.sub( farRadiance( direction ) ).mul( cosine ).toVar();

					correction.addAssign( change );

					if ( directionNode !== null ) directionNode.addAssign( direction.mul( luminance( change ).mul( 4 * Math.PI / radiosity.directions ) ) );

				} );

			} );

		} );

		return correction.mul( 4 * Math.PI / radiosity.directions );

	}

	/**
	 * Deposits the rays of a texel along the directions of the current sample.
	 *
	 * @param {RadianceCascades} cascades - The radiance cascades.
	 * @param {Node<vec3>} position - The world-space position.
	 * @param {Node<vec3>} normal - The world-space normal.
	 * @param {Node<vec3>} faceNormal - The face normal.
	 */
	trace( cascades, position, normal, faceNormal ) {

		const radiosity = this.radiosity;
		const probes = cascades.probesAt( position, faceNormal );
		const rays = radiosity._createNearest( position, faceNormal );

		Loop( radiosity.directions, ( { i } ) => {

			const direction = radiosity._directionNode.element( i ).toVar();

			If( dot( direction, faceNormal ).greaterThan( 0 ), () => {

				const { distance, radiance } = rays( i );

				cascades.depositHit( probes, direction, distance, radiance, position );

			} );

		} );

	}

}

/**
 * Returns the world-space triangles of the visible meshes with light map UVs, and the world-space size
 * of a texel of a light map of a size, from the area of the triangles over the area of their UVs.
 *
 * @private
 * @param {Scene} scene - The scene.
 * @param {number} size - The light map size.
 * @return {{triangles: Float32Array, groups: Uint8Array, meshes: Array<Mesh>, texelSize: number}} The triangles, the bit masks of their
 * orientations, see `groupsOf()`, the meshes and the texel size.
 */
function collectTriangles( scene, size ) {

	const positions = [];
	const groups = [];
	const meshes = [];
	let area = 0, uvArea = 0;

	scene.updateMatrixWorld();

	scene.traverseVisible( ( object ) => {

		if ( object.isMesh !== true || object.isSkinnedMesh || object.isBatchedMesh ) return;

		const geometry = object.geometry;
		const position = geometry.getAttribute( 'position' );
		const uv1 = geometry.getAttribute( 'uv1' );
		const normals = geometry.getAttribute( 'normal' );

		if ( uv1 === undefined ) return;

		meshes.push( object );

		const index = geometry.getIndex();
		const count = index !== null ? index.count : position.count;
		const start = Math.max( 0, geometry.drawRange.start );
		const end = Math.min( count, geometry.drawRange.start + geometry.drawRange.count );
		const instances = object.isInstancedMesh === true ? object.count : 1;
		const scale = object.isInstancedMesh === true ? geometry.getAttribute( 'instanceLightMapScale' ) : undefined;

		for ( let instance = 0; instance < instances; instance ++ ) {

			const matrixWorld = object.isInstancedMesh === true ? _matrix.multiplyMatrices( object.matrixWorld, object.getMatrixAt( instance, _matrix ) ) : object.matrixWorld;
			const uvScale = scale !== undefined ? scale.getX( instance ) * scale.getY( instance ) : 1;

			_normalMatrix.getNormalMatrix( matrixWorld );

			for ( let i = start; i + 2 < end; i += 3 ) {

				const v = [ 0, 1, 2 ].map( ( k ) => index !== null ? index.getX( i + k ) : i + k );
				const group = geometry.groups.find( ( { start: first, count: length } ) => i >= first && i < first + length );
				const material = Array.isArray( object.material ) ? object.material[ group?.materialIndex ?? 0 ] : object.material;

				_a.fromBufferAttribute( position, v[ 0 ] ).applyMatrix4( matrixWorld );
				_b.fromBufferAttribute( position, v[ 1 ] ).applyMatrix4( matrixWorld );
				_c.fromBufferAttribute( position, v[ 2 ] ).applyMatrix4( matrixWorld );

				positions.push( _a.x, _a.y, _a.z, _b.x, _b.y, _b.z, _c.x, _c.y, _c.z );

				// the face normal, on the side of the vertex normals like the face normals of the bake, which
				// flips the normals of back-sided materials

				_normal.subVectors( _b, _a ).cross( _vertexNormal.subVectors( _c, _a ) );
				area += _normal.length() / 2;

				if ( normals !== undefined ) {

					_vertexNormal.fromBufferAttribute( normals, v[ 0 ] );
					_vertexNormal.x += normals.getX( v[ 1 ] ) + normals.getX( v[ 2 ] );
					_vertexNormal.y += normals.getY( v[ 1 ] ) + normals.getY( v[ 2 ] );
					_vertexNormal.z += normals.getZ( v[ 1 ] ) + normals.getZ( v[ 2 ] );
					_vertexNormal.applyMatrix3( _normalMatrix );

					if ( _normal.dot( _vertexNormal ) < 0 ) _normal.negate();

				}

				if ( material?.side === BackSide ) _normal.negate();

				groups.push( groupsOf( _normal ) );

				const u0 = uv1.getX( v[ 0 ] ), w0 = uv1.getY( v[ 0 ] );
				const du1 = uv1.getX( v[ 1 ] ) - u0, dw1 = uv1.getY( v[ 1 ] ) - w0;
				const du2 = uv1.getX( v[ 2 ] ) - u0, dw2 = uv1.getY( v[ 2 ] ) - w0;

				uvArea += Math.abs( du1 * dw2 - du2 * dw1 ) / 2 * uvScale;

			}

		}

	} );

	const texelSize = uvArea > 0 ? Math.sqrt( area / ( uvArea * size * size ) ) : 1;

	return { triangles: new Float32Array( positions ), groups: new Uint8Array( groups ), meshes, texelSize };

}

/**
 * Returns the keys of the cells that the triangles overlap, with the separating axis test of "Fast 3D
 * Triangle-Box Overlap Testing" (Akenine-Möller, 2001). The cells are slightly enlarged, so points on
 * their faces count for both sides.
 *
 * @private
 * @param {Float32Array} triangles - The world-space triangles.
 * @param {Uint8Array} groups - The bit masks of the orientations of the triangles.
 * @param {Vector3} origin - The corner of the first cell.
 * @param {number} spacing - The cell size.
 * @param {Array<number>} bits - The bits of the key per axis.
 * @return {Set<number>} The keys.
 */
function voxelizeTriangles( triangles, groups, origin, spacing, bits ) {

	const keys = new Set();
	const half = 0.5 * ( 1 + 1e-3 );
	const v = [ new Float64Array( 3 ), new Float64Array( 3 ), new Float64Array( 3 ) ];
	const e = [ new Float64Array( 3 ), new Float64Array( 3 ), new Float64Array( 3 ) ];
	const p = [ new Float64Array( 3 ), new Float64Array( 3 ), new Float64Array( 3 ) ];
	const normal = new Float64Array( 3 );

	for ( let t = 0; t < triangles.length; t += 9 ) {

		// the triangle in cell units

		const min = [ Infinity, Infinity, Infinity ], max = [ - Infinity, - Infinity, - Infinity ];

		for ( let k = 0; k < 3; k ++ ) {

			for ( let axis = 0; axis < 3; axis ++ ) {

				const value = ( triangles[ t + k * 3 + axis ] - origin.getComponent( axis ) ) / spacing;
				v[ k ][ axis ] = value;
				min[ axis ] = Math.min( min[ axis ], value );
				max[ axis ] = Math.max( max[ axis ], value );

			}

		}

		for ( let axis = 0; axis < 3; axis ++ ) {

			e[ 0 ][ axis ] = v[ 1 ][ axis ] - v[ 0 ][ axis ];
			e[ 1 ][ axis ] = v[ 2 ][ axis ] - v[ 1 ][ axis ];
			e[ 2 ][ axis ] = v[ 0 ][ axis ] - v[ 2 ][ axis ];

		}

		normal[ 0 ] = e[ 0 ][ 1 ] * e[ 1 ][ 2 ] - e[ 0 ][ 2 ] * e[ 1 ][ 1 ];
		normal[ 1 ] = e[ 0 ][ 2 ] * e[ 1 ][ 0 ] - e[ 0 ][ 0 ] * e[ 1 ][ 2 ];
		normal[ 2 ] = e[ 0 ][ 0 ] * e[ 1 ][ 1 ] - e[ 0 ][ 1 ] * e[ 1 ][ 0 ];

		const planeRadius = half * ( Math.abs( normal[ 0 ] ) + Math.abs( normal[ 1 ] ) + Math.abs( normal[ 2 ] ) );

		for ( let z = Math.floor( min[ 2 ] - 1e-3 ); z <= Math.floor( max[ 2 ] + 1e-3 ); z ++ ) {

			for ( let y = Math.floor( min[ 1 ] - 1e-3 ); y <= Math.floor( max[ 1 ] + 1e-3 ); y ++ ) {

				for ( let x = Math.floor( min[ 0 ] - 1e-3 ); x <= Math.floor( max[ 0 ] + 1e-3 ); x ++ ) {

					const center = [ x + 0.5, y + 0.5, z + 0.5 ];

					for ( let k = 0; k < 3; k ++ ) for ( let axis = 0; axis < 3; axis ++ ) p[ k ][ axis ] = v[ k ][ axis ] - center[ axis ];

					// the plane of the triangle

					const distance = normal[ 0 ] * p[ 0 ][ 0 ] + normal[ 1 ] * p[ 0 ][ 1 ] + normal[ 2 ] * p[ 0 ][ 2 ];

					if ( Math.abs( distance ) > planeRadius ) continue;

					// the cross products of the edges and the axes of the cell

					let separated = false;

					for ( let i = 0; i < 3 && separated === false; i ++ ) {

						for ( let axis = 0; axis < 3; axis ++ ) {

							const a1 = ( axis + 1 ) % 3, a2 = ( axis + 2 ) % 3;
							const ax = [ 0, 0, 0 ];
							ax[ a1 ] = - e[ i ][ a2 ];
							ax[ a2 ] = e[ i ][ a1 ];

							const d0 = ax[ 0 ] * p[ 0 ][ 0 ] + ax[ 1 ] * p[ 0 ][ 1 ] + ax[ 2 ] * p[ 0 ][ 2 ];
							const d1 = ax[ 0 ] * p[ 1 ][ 0 ] + ax[ 1 ] * p[ 1 ][ 1 ] + ax[ 2 ] * p[ 1 ][ 2 ];
							const d2 = ax[ 0 ] * p[ 2 ][ 0 ] + ax[ 1 ] * p[ 2 ][ 1 ] + ax[ 2 ] * p[ 2 ][ 2 ];
							const radius = half * ( Math.abs( ax[ 0 ] ) + Math.abs( ax[ 1 ] ) + Math.abs( ax[ 2 ] ) );

							if ( Math.min( d0, d1, d2 ) > radius || Math.max( d0, d1, d2 ) < - radius ) {

								separated = true;
								break;

							}

						}

					}

					if ( separated === false && x >= 0 && y >= 0 && z >= 0 ) {

						for ( let group = 0; group < 6; group ++ ) {

							if ( groups[ t / 9 ] & ( 1 << group ) ) keys.add( encodeKey( x, y, z, bits, group ) );

						}

					}

				}

			}

		}

	}

	return keys;

}

function encodeKey( x, y, z, bits, group ) {

	return x + y * 2 ** bits[ 0 ] + z * 2 ** ( bits[ 0 ] + bits[ 1 ] ) + group * 2 ** ( bits[ 0 ] + bits[ 1 ] + bits[ 2 ] );

}

function decodeKey( key, bits ) {

	const x = key % 2 ** bits[ 0 ];
	const y = Math.floor( key / 2 ** bits[ 0 ] ) % 2 ** bits[ 1 ];
	const z = Math.floor( key / 2 ** ( bits[ 0 ] + bits[ 1 ] ) ) % 2 ** bits[ 2 ];
	const group = Math.floor( key / 2 ** ( bits[ 0 ] + bits[ 1 ] + bits[ 2 ] ) );

	return [ x, y, z, group ];

}

function shiftKey( key, bits, shift ) {

	const [ x, y, z, group ] = decodeKey( key, bits );

	return encodeKey( x >> shift, y >> shift, z >> shift, bits, group );

}

/**
 * Returns the orientation of a face normal: its dominant axis times two, plus one if it points to the
 * negative side.
 *
 * @private
 * @param {Node<vec3>} normal - The face normal.
 * @return {Node<uint>} The orientation.
 */
function groupOf( normal ) {

	const a = normal.abs().toVar();
	const axis = select( a.x.greaterThanEqual( a.y ).and( a.x.greaterThanEqual( a.z ) ), uint( 0 ), select( a.y.greaterThanEqual( a.z ), uint( 1 ), uint( 2 ) ) ).toVar();
	const component = select( axis.equal( 0 ), normal.x, select( axis.equal( 1 ), normal.y, normal.z ) );

	return axis.mul( 2 ).add( select( component.lessThan( 0 ), uint( 1 ), uint( 0 ) ) );

}

/**
 * Returns the bit mask of the orientations of a face normal, see `groupOf()`, with the near ties of the
 * axes, which the shaders may resolve either way.
 *
 * @private
 * @param {Vector3} normal - The face normal.
 * @return {number} The bit mask.
 */
function groupsOf( normal ) {

	const a = [ Math.abs( normal.x ), Math.abs( normal.y ), Math.abs( normal.z ) ];
	const largest = Math.max( a[ 0 ], a[ 1 ], a[ 2 ] );
	let mask = 0;

	for ( let axis = 0; axis < 3; axis ++ ) {

		if ( a[ axis ] >= largest * 0.999 ) mask |= 1 << ( axis * 2 + ( normal.getComponent( axis ) < 0 ? 1 : 0 ) );

	}

	return mask;

}

// the finalizer of MurmurHash3, which spreads the keys of neighboring cells over the table

function mixKey( key ) {

	let h = key >>> 0;
	h ^= h >>> 16;
	h = Math.imul( h, 0x85ebca6b );
	h ^= h >>> 13;
	h = Math.imul( h, 0xc2b2ae35 );
	h ^= h >>> 16;

	return h >>> 0;

}

function mixKeyNode( key ) {

	const h = uint( key ).toVar();
	h.assign( h.bitXor( h.shiftRight( uint( 16 ) ) ).mul( 0x85ebca6b ) );
	h.assign( h.bitXor( h.shiftRight( uint( 13 ) ) ).mul( 0xc2b2ae35 ) );

	return h.bitXor( h.shiftRight( uint( 16 ) ) );

}

/**
 * Returns the center direction of a bin of a cascade, see `RadianceCascades#_bin()`.
 *
 * @private
 * @param {Node<uint>} bin - The bin.
 * @param {number} theta - The bins of the cascade along the polar axis.
 * @return {Node<vec3>} The direction.
 */
function binDirection( bin, theta ) {

	const u = float( bin.mod( 2 * theta ) ).add( 0.5 ).div( 2 * theta );
	const z = float( bin.div( 2 * theta ) ).add( 0.5 ).div( theta ).mul( 2 ).sub( 1 ).toVar();
	const r = sqrt( z.mul( z ).oneMinus().max( 0 ) );
	const phi = u.mul( 2 * Math.PI ).toVar();

	return vec3( r.mul( cos( phi ) ), r.mul( sin( phi ) ), z );

}

/**
 * Returns the direction of equal-area coordinates, see "Algorithm 2" of the paper.
 *
 * @private
 */
function decodeDirection( u, v, target ) {

	const phi = u * 2 * Math.PI;
	const z = 2 * v - 1;
	const r = Math.sqrt( Math.max( 1 - z * z, 0 ) );

	return target.set( r * Math.cos( phi ), r * Math.sin( phi ), z );

}

export { RadianceCascades, radianceCascadesIrradiance, RasterCascadeSource };
