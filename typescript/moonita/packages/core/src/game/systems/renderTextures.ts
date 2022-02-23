import { State } from "../State";
import * as V from "../common/Vector";

export function syncPixiTransforms(state: State) {
  state.queries.pixiObject._forEach((eid) => {
    const pixiObject = state.components.pixiObject.ref[eid];
    const transform = state.components.transform[eid];
    const isSprite = state.components.pixiObject.scaleBySpriteDimenssions[eid];

    V.cloneInto(pixiObject.position, transform.position);

    if (isSprite) {
      pixiObject.width = transform.scale.x;
      pixiObject.height = transform.scale.y;
    } else {
      pixiObject.scale.x = transform.scale.x;
      pixiObject.scale.y = transform.scale.y;
    }

    pixiObject.rotation = transform.rotation;

    if (isSprite) pixiObject.rotation += Math.PI / 2;
  });
}
