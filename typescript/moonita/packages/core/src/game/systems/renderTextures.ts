import { LayerId, State, Vector2 } from "../State";
import { applyTransform } from "./renderWithTransform";
import * as C from "../common/Camera";
import * as T from "../common/Transform";
import * as V from "../common/Vector";
import * as AABB from "../common/AABB";
import { Flag } from "../common/Flags";
import { mat3, vec2 } from "gl-matrix";
import { TextureId } from "../assets";
import { computeTransformMatrix } from "../common/Entity";

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

function generateTextureMatrix(state: State, id: TextureId) {
  const t = state.assets[id];
  const m = mat3.create();

  // mat3.scale(m, m, [1 / 2, 1 / 2]);
  mat3.translate(m, m, [0.5, 0.5]);
  mat3.rotate(m, m, -t.inherentRotation);

  return m;
}

function genericTextureMatrices(state: State) {
  return state.assets.map((_, id) => generateTextureMatrix(state, id));
}

export const renderWebglSprites = (state: State) => {
  // TODO: don't do this...
  const matrices = genericTextureMatrices(state);
  for (let layer = 0; layer < state.components.layers.length; layer++) {
    state.queries.spriteLayers[layer]._forEach((eid) => {
      const textureMatrix = matrices[state.components.sprite.textureId[eid]];
      const transformMatrix =
        state.components.transformMatrix[eid] ||
        computeTransformMatrix(state, eid);
      const textureId = state.components.sprite.textureId[eid];

      renderGpuSprite(state, transformMatrix, textureId, textureMatrix);
    });
  }
};

export function renderGpuSprite(
  state: State,
  transformMatrix: mat3,
  textureId: TextureId,
  textureMatrix: mat3 = generateTextureMatrix(state, textureId)
) {
  const texture = state.textures[textureId];

  state.webglRenderers.spriteRenderer.draw(
    transformMatrix,
    texture,
    textureMatrix
  );
}
