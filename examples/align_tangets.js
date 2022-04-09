import * as THREE from 'three';

import { GUI } from './jsm/libs/lil-gui.module.min.js';
import { GLTFLoader } from './jsm/loaders/GLTFLoader.js';
import { VertexTangentsHelper } from './jsm/helpers/VertexTangentsHelper.js';

let scene, renderer;
let camera, light;
let vth, mesh, connections;

init();
animate();

function init() {

	renderer = new THREE.WebGLRenderer();
	renderer.setPixelRatio( window.devicePixelRatio );
	renderer.setSize( window.innerWidth, window.innerHeight );
	document.body.appendChild( renderer.domElement );

	//

	camera = new THREE.PerspectiveCamera( 70, window.innerWidth / window.innerHeight, 1, 1000 );
	camera.position.z = 400;

	scene = new THREE.Scene();

	light = new THREE.PointLight();
	light.position.set( 200, 100, 150 );
	scene.add( light );

	scene.add( new THREE.PointLightHelper( light, 15 ) );

    let settings = {
        'Compute Tangents from UVs': function () {
            mesh.geometry.computeTangents(); // generates bad data due to degenerate UVs
        },
        'Set Tangents Up': function () {
            let attributes = mesh.geometry.attributes;
            let positions  = attributes.position.array;
            let tangentAttribute = attributes.tangent;

            for (let i = 0; i < positions.length / 3; i++) {
                tangentAttribute.setXYZ(i, 0, 1, 0);
            }
            attributes.tangent.needsUpdate = true;

        },
        'Comb Tangents One Iteration': function () {

            let index      = mesh.geometry.index;
            let indices    = index.array;
            let attributes = mesh.geometry.attributes;
            let positions  = attributes.position.array;
            let normals    = attributes.normal.array;
            let tangentAttribute = attributes.tangent;
            let tangents   = tangentAttribute.array;
        
            // Step 1: Construct a table that connects all vertices to their neighbors
            connections      = new Array(positions.length / 3).fill([]);
            for (let i = 0; i < indices.length; i += 3) {
                let i0 = indices[i + 0];
                let i1 = indices[i + 1];
                let i2 = indices[i + 2];
                if (!connections[i0].includes(i1)) { connections[i0].push(i1); }
                if (!connections[i0].includes(i2)) { connections[i0].push(i2); }
                if (!connections[i1].includes(i0)) { connections[i1].push(i0); }
                if (!connections[i1].includes(i2)) { connections[i1].push(i2); }
                if (!connections[i2].includes(i0)) { connections[i2].push(i0); }
                if (!connections[i2].includes(i1)) { connections[i2].push(i1); }
            }
            
            // Step 2: For each vertex, make the current tangent the average of
            // the tangents of its neighbors (projected to the plane of the normal)
            let sumVector = new THREE.Vector3();
            let tempVec   = new THREE.Vector3();
            for (let i = 0; i < positions.length / 3; i++) {
                sumVector.set(0, 0, 0);
                for (let j = 0; j < connections[i].length; j++) {
                    sumVector.set(
                        sumVector.x + tangents[(connections[i][j] * 3) + 0],
                        sumVector.y + tangents[(connections[i][j] * 3) + 1],
                        sumVector.z + tangents[(connections[i][j] * 3) + 2]);
                }
                sumVector = sumVector.projectOnPlane(tempVec.set(
                    normals[(i * 3) + 0],
                    normals[(i * 3) + 1],
                    normals[(i * 3) + 2]));
                sumVector = sumVector.normalize();
        
                tangentAttribute.setXYZ(i, sumVector.x, sumVector.y, sumVector.z);
            }
            attributes.tangent.needsUpdate = true;

        }

    }
    const panel = new GUI( { width: 310 } );
    for (let name in settings) { panel.add(settings, name); }

	const loader = new GLTFLoader();
	loader.load( 'models/gltf/LeePerrySmith/LeePerrySmith.glb', function ( gltf ) {

		mesh = gltf.scene.children[ 0 ];

		mesh.geometry.computeTangents(); // generates bad data due to degenerate UVs

		const group = new THREE.Group();
		group.scale.multiplyScalar( 50 );
		scene.add( group );

		// To make sure that the matrixWorld is up to date for the boxhelpers
		group.updateMatrixWorld( true );

		group.add( mesh );

		vth = new VertexTangentsHelper( mesh, 5 );
		scene.add( vth );

	} );

	window.addEventListener( 'resize', onWindowResize );

}

function onWindowResize() {

	camera.aspect = window.innerWidth / window.innerHeight;
	camera.updateProjectionMatrix();

	renderer.setSize( window.innerWidth, window.innerHeight );

}

function animate() {

	requestAnimationFrame( animate );

	const time = - performance.now() * 0.0003;

	camera.position.x = 400 * Math.cos( time );
	camera.position.z = 400 * Math.sin( time );
	camera.lookAt( scene.position );

	light.position.x = Math.sin( time * 1.7 ) * 300;
	light.position.y = Math.cos( time * 1.5 ) * 400;
	light.position.z = Math.cos( time * 1.3 ) * 300;

	if ( vth ) vth.update();

	renderer.render( scene, camera );

}
