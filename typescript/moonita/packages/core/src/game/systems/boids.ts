import * as GameAction from "../GameAction";
import { getEntityVec, getPosition, getVelocity } from "../common/Entity";
import { settings } from "../common/Settings";
import * as V from "../common/Vector";
import { applyForce } from "../physics";
import { LayerId, State } from "../State";
import * as Segment from "../common/Segment";
import { renderLine } from "./renderLinePath";
import { Flag } from "../common/Flags";
import { renderCustomArrow } from "./debugArrows";
import { triggerEvent } from "./handleGameAction";
import { renderCircle } from "./basicRenderers";
import { vec4 } from "gl-matrix";
import { transformMatrixFromTransform } from "../common/Transform";

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

    V.attemptNormalizeMut(prediction, prediction);
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

    const outsidePath =
      minProjection && minProjection.lengthSquared > path.radius ** 2;

    if (state.flags[Flag.DebugShowPathfollowingProjections])
      state.components.pathFollowingBehavior.debugData.hasProjection[eid] =
        Number(minProjection);
    if (state.flags[Flag.DebugShowSelectedEntityPath])
      state.components.pathFollowingBehavior.debugData.followedSegment[eid] =
        minProjection ? minProjection!.segment : path.points.length;

    const lastPoint = path.points[path.points.length - 1].position;

    // When goal reached
    if (V.distanceSquared(lastPoint, position) <= path.goalRadius ** 2) {
      triggerEvent(state, GameAction.onPathfindingGoalReached(eid));
      state.ecs.removeComponent(eid, state.components.pathFollowingBehavior);
      console.log("removed");
    }

    if (!minProjection) return;

    const target = V.sub(
      path.points[minProjection.segment + 1].position,
      path.points[minProjection.segment].position
    );

    if (!outsidePath) {
      V.normalizeMut(target, target);
      V.scaleMut(target, target, predictionDistance * 3);

      if (state.flags[Flag.DebugShowPathfollowingProjections]) {
        const saveProjectionInto =
          state.components.pathFollowingBehavior.debugData.projection;

        saveProjectionInto.x[eid] = position.x + target.x;
        saveProjectionInto.y[eid] = position.y + target.y;
      }

      moveTowards(state, eid, target, settings.pathFollowingCoefficient / 4);
    }

    if (outsidePath) {
      V.normalizeMut(target, target);
      V.scaleMut(target, target, predictionDistance);
      V.addMut(target, target, minProjection.projection);
      V.subMut(target, target, position);

      moveTowards(state, eid, target, settings.pathFollowingCoefficient);

      if (state.flags[Flag.DebugShowPathfollowingProjections]) {
        const saveProjectionInto =
          state.components.pathFollowingBehavior.debugData.projection;

        saveProjectionInto.x[eid] = minProjection.projection.x;
        saveProjectionInto.y[eid] = minProjection.projection.y;
      }
    }

    if (state.flags[Flag.DebugShowPathfollowingForces]) {
      const saveInto = state.components.pathFollowingBehavior.debugData.force;

      saveInto.x[eid] = target.x;
      saveInto.y[eid] = target.y;
    }
  });
}

export function separate(state: State) {
  state.queries.boidSeparation._forEach((eid) => {
    const position = getPosition(state, eid);
    const total = V.origin();
    const team = state.components.team[eid];

    for (let teamId = 0; teamId < state.map.teams.length; teamId++) {
      const result = state.structures.boidQuadTrees[teamId].retrieve(
        position,
        teamId === team
          ? settings.separationRadius
          : settings.separationDifferentTeamRadius
      );

      for (let i = 0; i < result.used; i++) {
        const node = result.elements[i];

        if (node === eid) continue;

        const otherPosition = getPosition(state, node);
        const dist = V.distance(position, otherPosition);
        const coefficient =
          teamId === team ? 1 : settings.separationDiffereTeamCoefficient;

        V.subMut(otherPosition, position, otherPosition);
        V.normalizeMut(otherPosition, otherPosition);
        V.scaleMut(otherPosition, otherPosition, coefficient / dist);

        V.addMut(total, total, otherPosition);
      }
    }

    if (total.x || total.y) {
      moveTowards(state, eid, total, settings.separationCoefficient);
    }
  });
}

export function align(state: State) {
  state.queries.boidAlignment._forEach((eid) => {
    const team = state.components.team[eid];
    const position = getPosition(state, eid);
    const total = V.origin();

    const result = state.structures.boidQuadTrees[team].retrieve(
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
    const team = state.components.team[eid];
    const position = getPosition(state, eid);
    const total = V.origin();
    let count = 0;

    const result = state.structures.boidQuadTrees[team].retrieve(
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
  pathFollow(state);
}

export function renderDebugBoidData(state: State) {
  const context = state.contexts[LayerId.DebugLayer];
  context.save();
  if (state.flags[Flag.DebugShowPathfollowingProjections]) {
    context.strokeStyle = "black";
    context.lineWidth = 1;

    state.queries.boidPathFollowing._forEach((eid) => {
      if (state.components.pathFollowingBehavior.debugData.hasProjection[eid])
        return;

      const position = getPosition(state, eid);
      const pathFollowing = getEntityVec(
        state.components.pathFollowingBehavior.debugData.projection,
        eid
      );

      renderLine(context, position, pathFollowing);
    });
  }

  if (state.flags[Flag.DebugShowPathfollowingForces]) {
    context.lineWidth = 1;
    state.queries.boidPathFollowing._forEach((eid) => {
      if (!state.components.pathFollowingBehavior.debugData.hasProjection)
        return;
      const force = getEntityVec(
        state.components.pathFollowingBehavior.debugData.force,
        eid
      );

      if (force.x === 0 && force.y === 0) return;

      const position = getPosition(state, eid);
      V.scaleMut(force, force, 1000);
      V.addMut(force, force, position);

      renderCustomArrow(context, position, force, { x: 2, y: 1 });
    });
  }

  if (state.flags[Flag.DebugShowPathfollowingGoals]) {
    for (const path of state.paths) {
      const lastPoint = path.points[path.points.length - 1].position;

      // TODO: reimplement

      // state.webglRenderers.solidColorCircleRenderer.draw(
      //   transformMatrixFromTransform(
      //     lastPoint.x,
      //     lastPoint.y,
      //     path.goalRadius,
      //     path.goalRadius,
      //     0
      //   ),
      //   vec4.fromValues(0.8, 0.8, 0.8, 0.6)
      // );
    }
  }

  if (
    state.flags[Flag.DebugShowSelectedEntityPath] &&
    state.selectedEntity &&
    state.selectedEntity.isPathFollower
  ) {
    const eid = state.selectedEntity.id;
    const pathId = state.components.pathFollowingBehavior.path[eid];
    const path = state.paths[pathId];
    const segment =
      state.components.pathFollowingBehavior.debugData.followedSegment[eid];

    if (segment < path.points.length) {
      context.strokeStyle = "green";
      context.lineWidth = 4;

      renderLine(
        context,
        path.points[segment].position,
        path.points[segment + 1].position
      );
    }
  }
  context.restore();
}
