import * as twgl from "twgl.js";
import { ShaderId } from "../assets";
import { mat3, vec2, vec3, vec4 } from "gl-matrix";

interface GenericUniforms {
  u_transform_matrix: mat3;
  u_projection_matrix: mat3;
  u_world_matrix: mat3;
}

interface SpriteRendererUniforms extends GenericUniforms {
  u_texture_matrix: mat3;
  tex: WebGLTexture;
}

interface SolidQuadUniforms extends GenericUniforms {
  u_color: vec4;
}

function createUnitQuad(gl: WebGL2RenderingContext) {
  const bufferInfo = twgl.createBufferInfoFromArrays(gl, {
    a_position: {
      data: [-1, -1, -1, 1, 1, -1, 1, -1, -1, 1, 1, 1],
      numComponents: 2,
    },
  });

  return bufferInfo;
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
    programs: ReadonlyArray<WebGLProgram>
  ) {
    this.program = programs[ShaderId.SpriteShader];
    this.programInfo = twgl.createProgramInfoFromProgram(gl, this.program);

    const bufferInfo = createUnitQuad(gl);
    this.bufferInfo = bufferInfo;
    this.vaoInfo = twgl.createVertexArrayInfo(gl, this.programInfo, bufferInfo);
  }

  public draw(
    transformMatrix: mat3,
    texture: WebGLTexture,
    textureMatrix: mat3
  ) {
    this.gl.useProgram(this.program);

    const uniforms: SpriteRendererUniforms = {
      u_transform_matrix: transformMatrix,
      u_projection_matrix: this.projectionMatrix,
      u_world_matrix: this.worldMatrix,
      tex: texture,
      u_texture_matrix: textureMatrix,
    };

    twgl.setUniforms(this.programInfo, uniforms);

    this.gl.bindVertexArray(this.vaoInfo.vertexArrayObject!);
    this.gl.drawArrays(this.gl.TRIANGLES, 0, 6);
    this.gl.bindVertexArray(null);
  }
}

export class SolidColorQuadRenderer {
  private program: WebGLProgram;

  private bufferInfo: twgl.BufferInfo;
  private vaoInfo: twgl.VertexArrayInfo;
  private programInfo: twgl.ProgramInfo;

  public constructor(
    public gl: WebGL2RenderingContext,
    public projectionMatrix: mat3,
    public worldMatrix: mat3,
    programs: ReadonlyArray<WebGLProgram>,
    isCircle = false
  ) {
    this.program =
      programs[
        isCircle ? ShaderId.SolidColorCircleShader : ShaderId.SolidColorShader
      ];
    this.programInfo = twgl.createProgramInfoFromProgram(gl, this.program);

    const bufferInfo = createUnitQuad(gl);
    this.bufferInfo = bufferInfo;
    this.vaoInfo = twgl.createVertexArrayInfo(gl, this.programInfo, bufferInfo);
  }

  public draw(transformMatrix: mat3, color: vec4) {
    this.gl.useProgram(this.program);

    const uniforms: SolidQuadUniforms = {
      u_transform_matrix: transformMatrix,
      u_projection_matrix: this.projectionMatrix,
      u_world_matrix: this.worldMatrix,
      u_color: color,
    };

    twgl.setUniforms(this.programInfo, uniforms);

    this.gl.bindVertexArray(this.vaoInfo.vertexArrayObject!);
    this.gl.drawArrays(this.gl.TRIANGLES, 0, 6);
    this.gl.bindVertexArray(null);
  }
}
