import { State } from "../State";

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

    const texture = state.assets[textureId];
    const image = texture.image;
    const textureRotation = texture.inherentRotation;

    state.ctx.save();

    state.ctx.translate(x, y);
    state.ctx.rotate(rotation + textureRotation);
    state.ctx.scale(scaleX, scaleY);

    state.ctx.drawImage(image, -width / 2, -height / 2, width, height);

    state.ctx.restore();
  });
};
