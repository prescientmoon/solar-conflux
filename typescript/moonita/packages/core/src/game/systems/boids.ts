import { getPosition, getVelocity } from "../common/Entity";
import { settings } from "../common/Settings";
import * as V from "../common/Vector";
import { applyForce } from "../physics";
import { LayerId, State } from "../State";
import * as Segment from "../common/Segment";
import { renderLine } from "./renderLinePath";
import { Flag } from "../common/Flags";
import { renderCustomArrow } from "./debugArrows";

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

interface ProjectionWithLength {
  projection: V.Vector2;
  lengthSquared: number;
  segment: number;
}

function pathFollow(state: State) {
  state.queries.boidPathFollowing._forEach((eid) => {
    const path = state.paths[state.components.pathFollowingBehavior.path[eid]];
    const position = getPosition(state, eid);
    const prediction = getVelocity(state, eid);

    const predictionDistance = 30;

    V.normalizeMut(prediction, prediction);
    V.scaleMut(prediction, prediction, predictionDistance);
    V.addMut(prediction, prediction, position);

    let minProjection: ProjectionWithLength | null = null;

    for (let i = 1; i < path.points.length; i++) {
      const projection = Segment.projectPoint(
        {
          from: path.points[i - 1].position,
          to: path.points[i].position,
        },
        prediction
      );

      if (projection === null) continue;

      const distance = V.distanceSquared(projection, prediction);

      if (minProjection === null || minProjection!.lengthSquared > distance) {
        minProjection = {
          lengthSquared: distance,
          projection,
          segment: i - 1,
        };
      }
    }

    if (minProjection === null) return;

    if (minProjection.lengthSquared > path.radius ** 2) {
      const target = V.sub(
        path.points[minProjection.segment + 1].position,
        path.points[minProjection.segment].position
      );

      V.normalizeMut(target, target);
      V.scaleMut(target, target, predictionDistance);
      V.addMut(target, target, minProjection.projection);
      V.subMut(target, target, position);

      moveTowards(state, eid, target, settings.pathFollowingCoefficient);
    }
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
  pathFollow(state);
  separate(state);
  align(state);
  cohese(state);
  seek(state);
}
