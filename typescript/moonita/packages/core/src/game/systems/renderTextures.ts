import { createContext } from "preact";
import { State } from "../State";
import { applyTransform } from "./renderWithTransform";

export const renderTextures = (state: State) => {
  state.queries.textured._forEach((eid) => {
    const textureId = state.components.texture.textureId[eid];
    const width = state.components.texture.width[eid];
    const height = state.components.texture.height[eid];
    const x = state.components.transform.position.x[eid];
    const y = state.components.transform.position.y[eid];
    const rotation = state.components.transform.rotation[eid];
    const scaleX = state.components.transform.scale.x[eid];
    const scaleY = state.components.transform.scale.y[eid];

    renderTexture(
      state,
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

  state.ctx.save();

  applyTransform(state, x, y, rotation, scaleX, scaleY);
  state.ctx.rotate(textureRotation);

  state.ctx.drawImage(image, -width / 2, -height / 2, width, height);

  state.ctx.restore();
}
