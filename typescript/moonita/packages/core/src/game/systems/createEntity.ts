import * as GameAction from "../GameAction";
import * as PIXI from "pixi.js";
import { boidTextureByTeam, TextureId } from "../assets";
import { settings } from "../common/Settings";
import { Vector2 } from "../common/Vector";
import { LayerId, SimulationState, State } from "../State";
import { insertBoidIntoQuadTree } from "./boidQuadTree";
import { setEntityVec } from "../common/Entity";

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

export const addSprite = (
  state: State,
  eid: number,
  layer: LayerId,
  id: TextureId
) => {
  state.ecs.addComponent(eid, state.components.sprite);
  state.ecs.addComponent;

  const sprite = new PIXI.Sprite(state.pixiTextures[id]);
  const layerContainer = state.pixiStage.children[layer] as PIXI.Container;

  sprite.anchor.set(0.5, 0.5);

  state.components.sprite[eid] = sprite;
  layerContainer.addChild(sprite);
};

export const createBullet = (
  state: State,
  startFrom: number,
  velocity: number,
  lifetime: number
) => {
  const eid = state.ecs.createEntity();

  const positionX = state.components.transform.position.x[startFrom];
  const positionY = state.components.transform.position.y[startFrom];
  const rotation = state.components.transform.rotation[startFrom];

  markEntityCreation(state, eid);
  addSprite(state, eid, LayerId.BulletLayer, TextureId.BlueBullet);

  state.ecs.addComponent(eid, state.components.transform);
  state.ecs.addComponent(eid, state.components.velocity);
  state.ecs.addComponent(eid, state.components.acceleration);
  state.ecs.addComponent(eid, state.components.bullet);
  state.ecs.addComponent(eid, state.components.mortal);

  state.components.transform.position.x[eid] = positionX;
  state.components.transform.position.y[eid] = positionY;
  state.components.transform.rotation[eid] = rotation;

  unstretch(state, eid);
  setAcceleration(state, eid, 0, 0);

  state.components.velocity.x[eid] = Math.cos(rotation) * velocity;
  state.components.velocity.y[eid] = Math.sin(rotation) * velocity;
  state.components.mortal.lifetime[eid] = lifetime;

  state.tickScheduler.schedule(
    state.tick + lifetime,
    GameAction.despawnEntity(eid)
  );

  return eid;
};

export function defaultTransform(state: State, eid: number) {
  setPosition(state, eid, 0, 0);
  state.components.transform.scale.x[eid] = 1;
  state.components.transform.scale.y[eid] = 1;
  state.components.transform.rotation[eid] = 0;
}

export function setPosition(state: State, eid: number, x: number, y: number) {
  state.components.transform.position.x[eid] = x;
  state.components.transform.position.y[eid] = y;
}

export function setVelocity(state: State, eid: number, x: number, y: number) {
  state.components.velocity.x[eid] = x;
  state.components.velocity.y[eid] = y;
}

export function limitSpeed(state: State, eid: number, to: number) {
  state.components.speedLimit[eid] = to;
}

export function setAcceleration(
  state: SimulationState,
  eid: number,
  x: number,
  y: number
) {
  state.components.acceleration.x[eid] = x;
  state.components.acceleration.y[eid] = y;
}

export function createBoid(state: State, position: Vector2, team: number) {
  const eid = state.ecs.createEntity();

  state.ecs.addComponent(eid, state.components.transform);
  state.ecs.addComponent(eid, state.components.acceleration);
  state.ecs.addComponent(eid, state.components.velocity);
  state.ecs.addComponent(eid, state.components.physicsObject);

  state.ecs.addComponent(eid, state.components.speedLimit);
  state.ecs.addComponent(eid, state.components.boidAlignment);
  state.ecs.addComponent(eid, state.components.boidCohesion);
  state.ecs.addComponent(eid, state.components.boidSeparation);
  state.ecs.addComponent(eid, state.components.pathFollowingBehavior);

  state.ecs.addComponent(eid, state.components.sprite);
  state.ecs.addComponent(eid, state.components.rotateAfterVelocity);
  state.ecs.addComponent(eid, state.components.team);

  defaultTransform(state, eid);
  setPosition(state, eid, position.x, position.y);
  setEntityVec(state.components.transform.scale, eid, { x: 0.01, y: 0.01 });
  setVelocity(state, eid, 0, 0);
  setAcceleration(state, eid, 0, 0);
  limitSpeed(state, eid, settings.maxBoidVelocity);

  state.components.physicsObject.mass[eid] = 1;

  state.components.team[eid] = team;

  addSprite(state, eid, LayerId.BulletLayer, boidTextureByTeam[team]);

  // TODO: automate this process
  insertBoidIntoQuadTree(state, eid, team);

  state.components.pathFollowingBehavior.path[eid] = team;

  return eid;
}
