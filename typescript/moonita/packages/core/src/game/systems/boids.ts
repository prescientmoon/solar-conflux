// TODO: spatial partitioning

import Quadtree from "@timohausmann/quadtree-js";
import { getPosition, getVelocity } from "../common/Entity";
import { maxBoidRadius, settings } from "../common/Settings";
import * as V from "../common/Vector";
import { applyForce } from "../physics";
import { State } from "../State";
import { boidQuadTreeNode } from "./boidQuadTree";

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

    const nodes = state.structures.boidQuadTree.retrieve<BoidQuadTreeNode>(
      boidQuadTreeNode(state, eid, settings.separationRadius)
    );

    for (let index = 0; index < nodes.length; index++) {
      const node = nodes[index];
      if (node.eid === eid) continue;

      const otherPosition = V.clone(node);
      const dist = V.distanceSquared(position, otherPosition);

      if (dist === 0 || dist > settings.separationRadius) continue;

      V.subMut(otherPosition, position, otherPosition);
      V.normalizeMut(otherPosition, otherPosition);
      V.scaleMut(otherPosition, otherPosition, 1 / Math.sqrt(dist));

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

    const nodes = state.structures.boidQuadTree.retrieve<BoidQuadTreeNode>(
      boidQuadTreeNode(state, eid, settings.alignmentCoefficient)
    );

    for (let index = 0; index < nodes.length; index++) {
      const node = nodes[index];
      if (node.eid === eid) continue;

      const otherPosition = V.clone(node);
      const dist = V.distanceSquared(position, otherPosition);

      if (dist === 0 || dist > settings.alignmentRadius) continue;

      V.addMut(total, total, getVelocity(state, node.eid));
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

    const nodes = state.structures.boidQuadTree.retrieve<BoidQuadTreeNode>(
      boidQuadTreeNode(state, eid, settings.alignmentCoefficient)
    );

    for (let index = 0; index < nodes.length; index++) {
      const node = nodes[index];
      if (node.eid === eid) continue;

      const otherPosition = V.clone(node);
      const dist = V.distanceSquared(position, otherPosition);

      if (dist === 0 || dist > settings.cohesionRadius) continue;

      V.addMut(total, total, otherPosition);
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
  // align(state);
  // cohese(state);
}
