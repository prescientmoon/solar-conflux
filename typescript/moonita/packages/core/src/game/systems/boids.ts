import { getPosition, getVelocity } from "../common/Entity";
import { settings } from "../common/Settings";
import * as V from "../common/Vector";
import { applyForce } from "../physics";
import { LayerId, State } from "../State";

/** Move a boid in a given direction */
function moveTowards(
  state: State,
  entity: number,
  force: V.Vector2,
  coefficient: number
) {
  const velocity = getVelocity(state, entity);

  V.normalizeMut(force, force);
  V.scaleMut(force, force, settings.maxBoidVelocity);
  V.subMut(force, force, velocity);
  V.limitMagnitudeMut(force, force, settings.maxBoidForce);
  V.scaleMut(force, force, coefficient);

  applyForce(state, entity, force);
}

// ========== Behaviors
function seek(state: State) {
  state.queries.boidSeek._forEach((eid) => {
    const position = getPosition(state, eid);
    const target = {
      x: state.components.seekingBehavior.target.x[eid],
      y: state.components.seekingBehavior.target.y[eid],
    };

    V.subMut(target, target, position);

    moveTowards(state, eid, target, settings.seekingCoefficinet);
  });
}

function pathFollow(state: State) {
  state.queries.boidPathFollowing._forEach((eid) => {
    const path = state.paths[state.components.pathFollowingBehavior.path[eid]];
    const position = getPosition(state, eid);
    const target = {
      x: state.components.seekingBehavior.target.x[eid],
      y: state.components.seekingBehavior.target.y[eid],
    };

    V.subMut(target, target, position);

    applyForce(state, eid, target);
  });
}

export function separate(state: State) {
  state.queries.boidSeparation._forEach((eid) => {
    const position = getPosition(state, eid);
    const total = V.origin();

    const result = state.structures.boidQuadTree.retrieve(
      position,
      settings.separationRadius
    );

    const context = state.contexts[LayerId.Unclearable];

    for (let i = 0; i < result.used; i++) {
      const node = result.elements[i];

      if (node === eid) continue;

      const otherPosition = getPosition(state, node);
      const dist = V.distance(position, otherPosition);

      V.subMut(otherPosition, position, otherPosition);
      V.normalizeMut(otherPosition, otherPosition);
      V.scaleMut(otherPosition, otherPosition, 1 / dist);

      V.addMut(total, total, otherPosition);
    }

    if (total.x || total.y) {
      moveTowards(state, eid, total, settings.separationCoefficient);
    }
  });
}

export function align(state: State) {
  state.queries.boidAlignment._forEach((eid) => {
    const position = getPosition(state, eid);
    const total = V.origin();

    const result = state.structures.boidQuadTree.retrieve(
      position,
      settings.alignmentRadius
    );

    for (let i = 0; i < result.used; i++) {
      const node = result.elements[i];

      if (node === eid) continue;

      V.addMut(total, total, getVelocity(state, node));
    }

    if (total.x || total.y) {
      moveTowards(state, eid, total, settings.alignmentCoefficient);
    }
  });
}

export function cohese(state: State) {
  state.queries.boidCohesion._forEach((eid) => {
    const position = getPosition(state, eid);
    const total = V.origin();
    let count = 0;

    const result = state.structures.boidQuadTree.retrieve(
      position,
      settings.cohesionRadius
    );

    for (let i = 0; i < result.used; i++) {
      const node = result.elements[i];

      if (node === eid) continue;

      total.x += state.components.transform.position.x[node];
      total.y += state.components.transform.position.y[node];
      count++;
    }

    if (count) {
      V.scaleMut(total, total, 1 / count);
      V.subMut(total, total, position);

      if (!total.x && !total.y) return;

      moveTowards(state, eid, total, settings.cohesionCoefficient);
    }
  });
}

export function simulateBoids(state: State) {
  separate(state);
  align(state);
  cohese(state);
  seek(state);
}
