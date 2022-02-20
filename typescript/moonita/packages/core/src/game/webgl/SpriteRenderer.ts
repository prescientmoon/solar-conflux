import * as twgl from "twgl.js";
import { ShaderId } from "../assets";
import { mat3, vec2, vec3 } from "gl-matrix";

interface TextureRendererUniforms {
  tex: WebGLTexture;
  u_transform_matrix: mat3;
  u_projection_matrix: mat3;
  u_world_matrix: mat3;
  u_texture_matrix: mat3;
  u_layer: number;
}

export class SpriteRenderer {
  private program: WebGLProgram;

  private bufferInfo: twgl.BufferInfo;
  private vaoInfo: twgl.VertexArrayInfo;
  private programInfo: twgl.ProgramInfo;

  public constructor(
    public gl: WebGL2RenderingContext,
    public projectionMatrix: mat3,
    public worldMatrix: mat3,
    public maxLayer: number,
    programs: ReadonlyArray<WebGLProgram>
  ) {
    this.program = programs[ShaderId.SpriteShader];
    this.programInfo = twgl.createProgramInfoFromProgram(gl, this.program);

    const bufferInfo = twgl.createBufferInfoFromArrays(gl, {
      a_position: {
        data: [-1, -1, -1, 1, 1, -1, 1, -1, -1, 1, 1, 1],
        numComponents: 2,
      },
    });

    this.bufferInfo = bufferInfo;
    this.vaoInfo = twgl.createVertexArrayInfo(gl, this.programInfo, bufferInfo);
  }

  public draw(
    transformMatrix: mat3,
    texture: WebGLTexture,
    textureMatrix: mat3,
    layer: number
  ) {
    this.gl.useProgram(this.program);

    const uniforms: TextureRendererUniforms = {
      tex: texture,
      u_transform_matrix: transformMatrix,
      u_projection_matrix: this.projectionMatrix,
      u_world_matrix: this.worldMatrix,
      u_texture_matrix: textureMatrix,
      u_layer: Math.random(), // layer / this.maxLayer,
    };

    twgl.setUniforms(this.programInfo, uniforms);

    const offset = 0;
    const count = 6;

    const logV = (v: [number, number]) => {
      vec2.transformMat3(v, v, transformMatrix);
      vec2.transformMat3(v, v, this.projectionMatrix);

      console.log(v);
    };

    // logV([-1, -1]);
    // throw new Error();

    this.gl.bindVertexArray(this.vaoInfo.vertexArrayObject!);
    this.gl.drawArrays(this.gl.TRIANGLES, offset, count);
    this.gl.bindVertexArray(null);
  }
}
