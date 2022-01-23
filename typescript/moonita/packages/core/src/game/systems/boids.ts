// TODO: spatial partitioning

import { settings } from "../common/Settigs";
import * as V from "../common/Vector";
import { applyForce } from "../physics";
import { State } from "../State";

export function separate(state: State) {
  state.queries.boidSeparation._forEach((eid) => {
    const position = {
      x: state.components.transform.position.x[eid],
      y: state.components.transform.position.y[eid],
    };

    const velocity = {
      x: state.components.velocity.x[eid],
      y: state.components.velocity.y[eid],
    };

    const total = V.origin();

    state.queries.boidSeparation._forEach((oeid) => {
      if (oeid === eid) return;

      const otherPosition = {
        x: state.components.transform.position.x[oeid],
        y: state.components.transform.position.y[oeid],
      };

      const dist = V.distanceSquared(position, otherPosition);

      // TODO: revamp this while using the babel transform
      if (dist === 0 || dist > settings.separationRadius) return;

      V.subMut(otherPosition, position, otherPosition);
      V.normalizeMut(otherPosition, otherPosition);
      V.scaleMut(otherPosition, otherPosition, 1 / Math.sqrt(dist));

      V.addMut(total, total, otherPosition);
    });

    if (total.x || total.y) {
      V.normalizeMut(total, total);
      V.scaleMut(total, total, settings.maxBoidVelocity);

      V.subMut(total, total, velocity);
      V.limitMagnitudeMut(total, total, settings.maxBoidForce);

      V.scaleMut(total, total, settings.separationCoefficient);
      applyForce(state, eid, total);
    }
  });
}

export function align(state: State) {
  state.queries.boidAlignment._forEach((eid) => {
    const position = {
      x: state.components.transform.position.x[eid],
      y: state.components.transform.position.y[eid],
    };

    const velocity = {
      x: state.components.velocity.x[eid],
      y: state.components.velocity.y[eid],
    };

    const total = V.origin();

    state.queries.boidAlignment._forEach((oeid) => {
      if (oeid === eid) return;

      const otherPosition = {
        x: state.components.transform.position.x[oeid],
        y: state.components.transform.position.y[oeid],
      };

      const dist = V.distanceSquared(position, otherPosition);

      // TODO: revamp this while using the babel transform
      if (dist === 0 || dist > settings.alignmentRadius) return;

      V.addMut(total, total, {
        x: state.components.velocity.x[oeid],
        y: state.components.velocity.y[oeid],
      });
    });

    if (total.x || total.y) {
      V.normalizeMut(total, total);
      V.scaleMut(total, total, settings.maxBoidVelocity);

      V.subMut(total, total, velocity);
      V.limitMagnitudeMut(total, total, settings.maxBoidForce);

      V.scaleMut(total, total, settings.alignmentCoefficient);
      applyForce(state, eid, total);
    }
  });
}

export function cohese(state: State) {
  state.queries.boidCohesion._forEach((eid) => {
    const position = {
      x: state.components.transform.position.x[eid],
      y: state.components.transform.position.y[eid],
    };

    const velocity = {
      x: state.components.velocity.x[eid],
      y: state.components.velocity.y[eid],
    };

    const total = V.origin();
    let count = 0;

    state.queries.boidCohesion._forEach((oeid) => {
      if (oeid === eid) return;

      const otherPosition = {
        x: state.components.transform.position.x[oeid],
        y: state.components.transform.position.y[oeid],
      };

      const dist = V.distanceSquared(position, otherPosition);

      // TODO: revamp this while using the babel transform
      if (dist === 0 || dist > settings.cohesionRadius) return;

      V.addMut(total, total, otherPosition);
      count++;
    });

    if (count) {
      V.scaleMut(total, total, 1 / count);
      V.subMut(total, total, position);

      if (!total.x && !total.y) return;

      V.normalizeMut(total, total);
      V.scaleMut(total, total, settings.maxBoidVelocity);

      V.subMut(total, total, velocity);
      V.limitMagnitudeMut(total, total, settings.maxBoidForce);

      V.scaleMut(total, total, settings.cohesionCoefficient);
      applyForce(state, eid, total);
    }
  });
}

export function simulateBoids(state: State) {
  separate(state);
  align(state);
  cohese(state);
}
