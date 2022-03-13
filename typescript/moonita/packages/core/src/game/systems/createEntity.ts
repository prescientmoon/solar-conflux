import * as GameAction from "../GameAction";
import * as PIXI from "pixi.js";
import * as V from "../common/Vector";
import { boidTextureByTeam, TextureId } from "../assets";
import { settings } from "../common/Settings";
import { Vector2 } from "../common/Vector";
import {
  EntityId,
  LayerId,
  SimulationState,
  State,
  stateIsComplete,
} from "../State";
import { insertBoidIntoQuadTree } from "./boidQuadTree";
import { identityTransform } from "../common/Transform";
import { BarOptions, createBar } from "../ui";
import { getLayer } from "../common/Entity";

/** Attach a sprite onto an entity */
export const addSprite = (
  state: State,
  eid: number,
  layer: LayerId,
  id: TextureId,
  name: null | string = null
) => {
  state.ecs.addComponent(eid, state.components.pixiObject);

  const sprite = new PIXI.Sprite(state.pixiTextures[id]);
  const layerContainer = getLayer(state, layer);

  sprite.anchor.set(0.5, 0.5);
  layerContainer.addChild(sprite);

  if (name) sprite.name = name;

  state.components.pixiObject.ref[eid] = sprite;
  state.components.pixiObject.scaleBySpriteDimenssions[eid] = Number(true);

  state.tickScheduler.scheduleEvent(
    GameAction.onDespawn(eid), // when the entity get's despawned..
    GameAction.clearSprite(eid) // ...delete it's sprite
  );
};

export const createBullet = (
  state: SimulationState,
  startFrom: number,
  velocityScalar: number,
  lifetime: number
) => {
  const eid = state.ecs.createEntity();

  const sourceTransform = state.components.transform[startFrom];

  state.ecs.addComponent(eid, state.components.transform);
  state.ecs.addComponent(eid, state.components.velocity);
  state.ecs.addComponent(eid, state.components.acceleration);
  state.ecs.addComponent(eid, state.components.bullet);
  state.ecs.addComponent(eid, state.components.mortal);

  const transform = identityTransform();

  state.components.transform[eid] = transform;

  V.scaleMut(transform.scale, transform.scale, 10);
  transform.position = V.clone(sourceTransform.position);
  transform.rotation = sourceTransform.rotation;

  const velocity = V.xBasis();

  V.rotateMut(velocity, velocity, sourceTransform.rotation);
  V.scaleMut(velocity, velocity, velocityScalar);

  state.components.acceleration[eid] = V.origin();
  state.components.velocity[eid] = velocity;
  state.components.mortal.lifetime[eid] = lifetime;

  state.tickScheduler.schedule(
    state.tick + lifetime,
    GameAction.despawnEntity(eid)
  );

  if (stateIsComplete(state)) {
    addSprite(state, eid, LayerId.BulletLayer, TextureId.BlueBullet, "Bullet");
  }

  return eid;
};

export function limitSpeed(state: SimulationState, eid: number, to: number) {
  state.components.speedLimit[eid] = to;
}

export function createUiBar(
  state: State,
  parent: EntityId,
  uiBarOptions: BarOptions
) {
  const eid = state.ecs.createEntity();

  state.ecs.addComponent(eid, state.components.uiBar);
  state.ecs.addComponent(eid, state.components.pixiObject);
  state.ecs.addComponent(eid, state.components.transform);
  state.ecs.addComponent(eid, state.components.positionOnlyChild);

  state.components.positionOnlyChild.childOf[eid] = parent;
  state.components.positionOnlyChild.offset[eid] = V.origin();

  state.components.uiBar[eid] = 255;
  state.components.pixiObject.scaleBySpriteDimenssions[eid] = 0;
  state.components.transform[eid] = identityTransform();

  state.components.pixiObject.ref[eid] = createBar(
    getLayer(state, LayerId.UiLayer),
    uiBarOptions
  );

  return eid;
}

// {{{
export function createBoid(
  state: SimulationState,
  position: Vector2,
  team: number
) {
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

  state.ecs.addComponent(eid, state.components.rotateAfterVelocity);
  state.ecs.addComponent(eid, state.components.team);

  // Position
  const transform = identityTransform();

  transform.scale.x = 15;
  transform.scale.y = 15;

  state.components.transform[eid] = transform;
  V.cloneInto(transform.position, position);

  limitSpeed(state, eid, settings.maxBoidVelocity);

  state.components.velocity[eid] = V.origin();

  // Mass & team
  state.components.physicsObject.mass[eid] = 1;
  state.components.team[eid] = team;
  state.components.pathFollowingBehavior.path[eid] = team;

  // TODO: automate this process?
  insertBoidIntoQuadTree(state, eid, team);

  if (stateIsComplete(state)) {
    addSprite(state, eid, LayerId.BulletLayer, boidTextureByTeam[team], "Boid");
  }

  return eid;
}
// }}}
