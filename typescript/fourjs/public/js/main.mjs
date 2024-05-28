import {GL} from "./gl.mjs";
import {ShaderCompiler} from "./shader.mjs"

const renderer = new GL();
renderer.init();

const compiler = new ShaderCompiler(renderer);
compiler.getShader("http://localhost:8000/fragment")
compiler.getShader("http://localhost:8000/vertex")
//
// const vShaderTxt = ShaderUtil.load("vertex_shader"),
//     fShaderTxt	= ShaderUtil.domShaderSrc("fragment_shader"),
//     // 2. Compile text and validate
//     vShader		= ShaderUtil.createShader(gl,vShaderTxt,gl.VERTEX_SHADER),
//     fShader		= ShaderUtil.createShader(gl,fShaderTxt,gl.FRAGMENT_SHADER),
//     // 3. Link the shaders together as a program.
//     shaderProg	= ShaderUtil.createProgram(gl,vShader,fShader,true);
//
//
//
// 				// 4. Get Location of Uniforms and Attributes.
// 				gl.useProgram(shaderProg);
// 				var aPositionLoc	= gl.getAttribLocation(shaderProg,"a_position"),
// 					uPointSizeLoc	= gl.getUniformLocation(shaderProg,"uPointSize");
// 				gl.useProgram(null);
// 				//............................................
// 				//Set Up Data Buffers
// 				var aryVerts = new Float32Array([0,0,0, 0.5,0.5,0 ]),
// 					bufVerts = gl.createBuffer();
// 				gl.bindBuffer(gl.ARRAY_BUFFER,bufVerts);
// 				gl.bufferData(gl.ARRAY_BUFFER, aryVerts, gl.STATIC_DRAW);
// 				gl.bindBuffer(gl.ARRAY_BUFFER,null);
// 				//............................................
// 				//Set Up For Drawing
// 				gl.useProgram(shaderProg);				//Activate the Shader
// 				gl.uniform1f(uPointSizeLoc,50.0);		//Store data to the shader's uniform variable uPointSize
// 				//how its down without VAOs
// 				gl.bindBuffer(gl.ARRAY_BUFFER,bufVerts);					//Tell gl which buffer we want to use at the moment
// 				gl.enableVertexAttribArray(aPositionLoc);					//Enable the position attribute in the shader
// 				gl.vertexAttribPointer(aPositionLoc,3,gl.FLOAT,false,0,0);	//Set which buffer the attribute will pull its data from
// 				gl.bindBuffer(gl.ARRAY_BUFFER,null);						//Done setting up the buffer
//
// 				this.gl.drawArrays(gl.POINTS, 0, 2);						//Draw the points
// 			});
