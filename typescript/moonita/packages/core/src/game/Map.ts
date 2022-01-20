import { Vector2 } from "./common/Vector";

// ========== Types
export interface PathPoint {
  position: Vector2;
}

export type CreaturePath = Array<PathPoint>;

export interface Team {
  base: Vector2 & { rotation: number };
  hostileTo: number;
  creaturePath: CreaturePath;
}

export interface Map {
  teams: Team[];
}

// ========== Constants
export const basicMap: Map = (() => {
  const playerTeam: Team = {
    base: {
      x: -1000,
      y: -1000,
      rotation: Math.PI / 4,
    },
    hostileTo: 2,
    creaturePath: [
      {
        position: {
          x: 100,
          y: 0,
        },
      },
      {
        position: {
          x: 1000,
          y: 1000,
        },
      },
    ],
  };

  const aiTeam: Team = {
    base: {
      x: 1000,
      y: 1000,
      rotation: (5 * Math.PI) / 4,
    },
    hostileTo: 1,
    creaturePath: [
      {
        position: {
          x: 0,
          y: 100,
        },
      },
      {
        position: {
          x: -1000,
          y: -1000,
        },
      },
    ],
  };

  return {
    teams: [playerTeam, aiTeam],
  };
})();

export const baseSize = 800;
