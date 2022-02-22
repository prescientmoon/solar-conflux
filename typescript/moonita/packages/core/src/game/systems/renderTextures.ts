import { LayerId, State } from "../State";
import { applyTransform } from "./renderWithTransform";
import * as C from "../common/Camera";
import * as T from "../common/Transform";
import * as V from "../common/Vector";
import * as AABB from "../common/AABB";
import { Flag } from "../common/Flags";

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

export function syncPixiTransforms(state: State) {
  state.queries.sprite._forEach((eid) => {
    const sprite = state.components.sprite[eid];

    sprite.position.x = state.components.transform.position.x[eid];
    sprite.position.y = state.components.transform.position.y[eid];

    sprite.width = state.components.transform.scale.x[eid];
    sprite.height = state.components.transform.scale.y[eid];

    sprite.rotation = state.components.transform.rotation[eid] + Math.PI / 2;
  });
}
