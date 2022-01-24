import Quadtree from "@timohausmann/quadtree-js";
import { getPosition, getVelocity } from "../common/Entity";
import { settings } from "../common/Settings";
import * as V from "../common/Vector";
import { applyForce } from "../physics";
import { State } from "../State";

interface BoidQuadTreeNode extends Quadtree.Rect {
  eid: number;
}

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

export function separate(state: State) {
  state.queries.boidSeparation._forEach((eid) => {
    const position = getPosition(state, eid);
    const total = V.origin();

    state.structures.boidQuadTree.retrieve(
      position,
      settings.separationRadius,
      (node) => {
        if (node.id === eid) return;

        const otherPosition = V.clone(node.position);
        const dist = V.distanceSquared(position, otherPosition);

        V.subMut(otherPosition, position, otherPosition);
        V.normalizeMut(otherPosition, otherPosition);
        V.scaleMut(otherPosition, otherPosition, 1 / Math.sqrt(dist));

        V.addMut(total, total, otherPosition);
      }
    );

    if (total.x || total.y) {
      moveTowards(state, eid, total, settings.separationCoefficient);
    }
  });
}

export function align(state: State) {
  state.queries.boidAlignment._forEach((eid) => {
    const position = getPosition(state, eid);
    const total = V.origin();

    state.structures.boidQuadTree.retrieve(
      position,
      settings.alignmentRadius,
      (node) => {
        if (node.id === eid) return;

        V.addMut(total, total, getVelocity(state, node.id));
      }
    );

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

    state.structures.boidQuadTree.retrieve(
      position,
      settings.alignmentRadius,
      (node) => {
        if (node.id === eid) return;

        total.x += node.position.x;
        total.y += node.position.y;
        count++;
      }
    );

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
}
