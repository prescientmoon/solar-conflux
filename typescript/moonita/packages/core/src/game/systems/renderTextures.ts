import { LayerId, State, Vector2 } from "../State";
import { applyTransform } from "./renderWithTransform";
import * as C from "../common/Camera";
import * as T from "../common/Transform";
import * as V from "../common/Vector";
import * as AABB from "../common/AABB";
import * as twgl from "twgl.js";
import { Flag } from "../common/Flags";
import { ShaderId } from "../assets";
import { mat3 } from "gl-matrix";

export const renderTextures = (state: State) => {
  const screen: AABB.AABB = {
    position: V.origin(),
    size: V.scalePerAxis(state.screenTransform.position, { x: 2, y: -2 }),
  };

  state.queries.textured._forEach((eid) => {
    const textureId = state.components.texture.textureId[eid];
    const width = state.components.texture.width[eid];
    const height = state.components.texture.height[eid];
    const layer = state.components.texture.layer[eid];
    const x = state.components.transform.position.x[eid];
    const y = state.components.transform.position.y[eid];
    const rotation = state.components.transform.rotation[eid];
    const scaleX = state.components.transform.scale.x[eid];
    const scaleY = state.components.transform.scale.y[eid];

    let shouldRender = true;

    if (state.flags[Flag.TextureCulling]) {
      const transform = {
        rotation,
        position: { x, y },
        scale: { x: scaleX, y: scaleY },
      };

      const screenPositionMin = C.toGlobalCoordinates(
        state.screenTransform,
        C.toGlobalCoordinates(
          state.camera,
          T.toGlobalCoordinates(transform, {
            x: -width / 2,
            y: -height / 2,
          })
        )
      );

      const screenPositionMax = C.toGlobalCoordinates(
        state.screenTransform,
        C.toGlobalCoordinates(
          state.camera,
          T.toGlobalCoordinates(transform, {
            x: width / 2,
            y: height / 2,
          })
        )
      );

      if (
        !AABB.pointInside(screen, screenPositionMax) &&
        !AABB.pointInside(screen, screenPositionMin)
      )
        shouldRender = false;
    }

    if (shouldRender)
      renderTexture(
        state,
        layer,
        textureId,
        x,
        y,
        rotation,
        scaleX,
        scaleY,
        width,
        height
      );
  });
};

export function renderTexture(
  state: State,
  layer: LayerId,
  textureId: number,
  x: number,
  y: number,
  rotation: number,
  scaleX: number,
  scaleY: number,
  width: number,
  height: number
) {
  const texture = state.assets[textureId];
  const image = texture.image;
  const textureRotation = texture.inherentRotation;
  const context = state.contexts[layer];

  context.save();

  applyTransform(context, x, y, rotation + textureRotation, scaleX, scaleY);

  context.drawImage(
    image,
    Math.floor(-width / 2),
    Math.floor(-height / 2),
    Math.floor(width),
    Math.floor(height)
  );

  context.restore();
}

interface TextureRendererUniforms {
  texture: WebGLTexture;
  u_transform_matrix: mat3;
  u_projection_matrix: mat3;
  u_texture_matrix: mat3;
}

export class TextureRenderer {
  private program: WebGLProgram;

  private vao: WebGLVertexArrayObject;
  private programInfo: twgl.ProgramInfo;
  private bufferInfo: twgl.BufferInfo;

  public constructor(
    public gl: WebGL2RenderingContext,
    public projectionMatrix: mat3,
    programs: ReadonlyArray<WebGLProgram>
  ) {
    this.program = programs[ShaderId.SpriteShader];
    this.programInfo = twgl.createProgramInfoFromProgram(gl, this.program);

    const vao = this.gl.createVertexArray();

    if (!vao) throw new Error(`Failed to create vao`);

    this.vao = vao;
    this.gl.bindVertexArray(vao);

    this.bufferInfo = twgl.createBufferInfoFromArrays(gl, {
      a_position: [0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1],
    });

    twgl.setBuffersAndAttributes(this.gl, this.programInfo, this.bufferInfo);

    this.gl.bindVertexArray(null);
  }

  public draw(
    transformMatrix: mat3,
    texture: WebGLTexture,
    textureMatrix: mat3
  ) {
    this.gl.bindVertexArray(this.vao);
    this.gl.useProgram(this.program);

    const uniforms: TextureRendererUniforms = {
      texture,
      u_transform_matrix: transformMatrix,
      u_projection_matrix: this.projectionMatrix,
      u_texture_matrix: textureMatrix,
    };

    twgl.setUniforms(this.programInfo, uniforms);

    const offset = 0;
    const count = 6;

    this.gl.drawArrays(this.gl.TRIANGLES, offset, count);
  }
}
