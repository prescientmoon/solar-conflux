import { TextureId } from "../assets";
import { State } from "../State";

export const markEntityCreation = (state: State, eid: number) => {
  state.ecs.addComponent(eid, state.components.created);
  state.components.created.createdAt[eid] = state.tick;
};

/**
 * Reset the scaling of an entity
 */
export const unstretch = (state: State, entity: number) => {
  state.components.transform.scale.x[entity] = 1;
  state.components.transform.scale.y[entity] = 1;
};

export const createBullet = (
  state: State,
  startFrom: number,
  velocityX: number,
  velocityY: number,
  lifetime: number
) => {
  const eid = state.ecs.createEntity();

  const positionX = state.components.transform.position.x[startFrom];
  const positionY = state.components.transform.position.y[startFrom];
  const rotation = state.components.transform.rotation[startFrom];

  markEntityCreation(state, eid);

  state.ecs.addComponent(eid, state.components.transform);
  state.ecs.addComponent(eid, state.components.velocity);
  state.ecs.addComponent(eid, state.components.bullet);
  state.ecs.addComponent(eid, state.components.mortal);
  state.ecs.addComponent(eid, state.components.texture);

  state.components.transform.position.x[eid] = positionX;
  state.components.transform.position.y[eid] = positionY;
  state.components.transform.rotation[eid] = rotation;
  unstretch(state, eid);

  state.components.velocity.x[eid] = velocityX;
  state.components.velocity.y[eid] = velocityY;
  state.components.mortal.lifetime[eid] = lifetime;

  state.components.texture.textureId[eid] = TextureId.BlueBullet;
  state.components.texture.width[eid] = 30;
  state.components.texture.height[eid] = 30;

  return eid;
};
