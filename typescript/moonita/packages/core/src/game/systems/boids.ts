import * as GameAction from "../GameAction";
import * as V from "../common/Vector";
import * as Segment from "../common/Segment";
import { getPosition, getVelocity } from "../common/Entity";
import { settings } from "../common/Settings";
import { applyForce } from "../physics";
import { SimulationState, State } from "../State";
import { renderLine } from "./renderLinePath";
import { Flag } from "../common/Flags";
import { renderCustomArrow } from "./debugArrows";
import { triggerEvent } from "./handleGameAction";
import { renderCircle } from "./basicRenderers";

/** Move a boid in a given direction */
function moveTowards(
  state: SimulationState,
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
function seek(state: SimulationState) {
  // Reuse the same vector for applying forces
  const result = V.origin();

  state.queries.boidSeek._forEach((eid) => {
    const position = getPosition(state, eid);
    const target = state.components.seekingBehavior.target[eid];

    V.subMut(result, target, position);

    moveTowards(state, eid, result, settings.seekingCoefficinet);
  });
}

interface ProjectionWithLength {
  projection: V.Vector2;
  lengthSquared: number;
  segment: number;
}

function pathFollow(state: SimulationState) {
  const prediction = V.origin();

  state.queries.boidPathFollowing._forEach((eid) => {
    const path = state.paths[state.components.pathFollowingBehavior.path[eid]];
    const position = getPosition(state, eid);
    const velocity = getVelocity(state, eid);

    const predictionDistance = 30;

    V.attemptNormalizeMut(prediction, velocity);
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

    if (
      state.flags[Flag.DebugShowPathfollowingForces] ||
      state.flags[Flag.DebugShowSelectedEntityPath] ||
      state.flags[Flag.DebugShowPathfollowingProjections]
    ) {
      state.components.pathFollowingBehavior.debugData.hasProjection[eid] =
        Number(minProjection);
    }
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
          state.components.pathFollowingBehavior.debugData.projection[eid];

        V.addMut(saveProjectionInto, position, target);
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
          state.components.pathFollowingBehavior.debugData.projection[eid];

        V.cloneInto(saveProjectionInto, minProjection.projection);
      }
    }

    if (state.flags[Flag.DebugShowPathfollowingForces]) {
      const saveInto =
        state.components.pathFollowingBehavior.debugData.force[eid];

      V.cloneInto(saveInto, target);
    }
  });
}

export function separate(state: SimulationState) {
  // Reuse temporary vector
  const positionDelta = V.origin();

  state.queries.boidSeparation._forEach((eid) => {
    const position = getPosition(state, eid);
    const total = V.origin();
    const differentTeamTotal = V.origin();
    const team = state.components.team[eid];

    for (let teamId = 0; teamId < state.map.teams.length; teamId++) {
      const result = state.structures.boidQuadTrees[teamId].retrieve(
        position,
        teamId === team
          ? settings.separationRadius
          : settings.separationDifferentTeamRadius
      );

      const saveInto = teamId === team ? total : differentTeamTotal;

      for (let i = 0; i < result.used; i++) {
        const node = result.elements[i];

        if (node === eid) continue;

        const otherPosition = getPosition(state, node);
        const dist = V.distance(position, otherPosition);
        const coefficient =
          teamId === team
            ? 1
            : settings.separationDifferentTeamDistanecMultiplier;

        V.subMut(positionDelta, position, otherPosition);
        V.normalizeMut(positionDelta, positionDelta);
        V.scaleMut(positionDelta, positionDelta, coefficient / dist);

        V.addMut(saveInto, saveInto, positionDelta);
      }
    }

    if (total.x || total.y) {
      moveTowards(state, eid, total, settings.separationCoefficient);
    }

    if (differentTeamTotal.x || differentTeamTotal.y) {
      moveTowards(
        state,
        eid,
        differentTeamTotal,
        settings.separationCoefficient *
          settings.separationDiffereTeamCoefficient
      );
    }

    if (state.flags[Flag.DebugShowBoidSeparationForces]) {
      V.cloneInto(
        state.components.boidSeparation.debugData.force[eid],
        V.add(total, differentTeamTotal)
      );
    }
  });
}

export function align(state: SimulationState) {
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

export function cohese(state: SimulationState) {
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

      V.addMut(total, total, state.components.transform[eid].position);
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
  const context = state.context;

  context.save();
  if (state.flags[Flag.DebugShowPathfollowingProjections]) {
    context.strokeStyle = "black";
    context.lineWidth = 1;

    state.queries.boidPathFollowing._forEach((eid) => {
      if (state.components.pathFollowingBehavior.debugData.hasProjection[eid])
        return;

      const position = getPosition(state, eid);
      const pathFollowing =
        state.components.pathFollowingBehavior.debugData.projection[eid];

      if (position === undefined || pathFollowing === undefined) return;

      renderLine(context, position, pathFollowing);
    });
  }

  if (
    state.flags[Flag.DebugShowPathfollowingForces] ||
    state.flags[Flag.DebugShowBoidSeparationForces]
  ) {
    const temp = V.origin(); // reuse vector

    function renderForce(eid: number, force: V.Vector2) {
      if (force.x === 0 && force.y === 0) return;

      const position = getPosition(state, eid);

      V.scaleMut(temp, force, 1000);
      V.addMut(temp, temp, position);

      renderCustomArrow(context, position, temp, { x: 2, y: 1 });
    }

    context.lineWidth = 1;
    if (state.flags[Flag.DebugShowPathfollowingForces])
      state.queries.boidPathFollowing._forEach((eid) => {
        if (!state.components.pathFollowingBehavior.debugData.hasProjection)
          return;

        const force =
          state.components.pathFollowingBehavior.debugData.force[eid];

        renderForce(eid, force);
      });
    if (state.flags[Flag.DebugShowBoidSeparationForces])
      state.queries.boidPathFollowing._forEach((eid) => {
        const force = state.components.boidSeparation.debugData.force[eid];

        renderForce(eid, force);
      });
  }

  if (state.flags[Flag.DebugShowPathfollowingGoals]) {
    context.fillStyle = "rgba(100,100,100,0.3)";
    context.save();
    for (const path of state.paths) {
      const lastPoint = path.points[path.points.length - 1].position;

      renderCircle(context, lastPoint.x, lastPoint.y, path.goalRadius);
      context.fill();
    }
    context.restore();
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

  if (state.flags[Flag.DebugShowBoidSeparationBonds]) {
    context.lineWidth = 1;
    state.queries.boidSeparation._forEach((eid) => {
      const team = state.components.team[eid];
      const position = getPosition(state, eid);

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

          renderLine(context, position, getPosition(state, node));
        }
      }
    });
  }

  context.restore();
}
