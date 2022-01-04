import { Vector2 } from "./common/types";

// ========== Types
export interface PathPoint {
  position: Vector2;
}

export type CreaturePath = Array<PathPoint>;

export interface Team {
  base: Vector2;
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
      x: 0,
      y: 1000,
    },
    hostileTo: 2,
    creaturePath: [
      {
        position: {
          x: 1000,
          y: 0,
        },
      },
    ],
  };

  const aiTeam: Team = {
    base: {
      x: 1000,
      y: 0,
    },
    hostileTo: 1,
    creaturePath: [
      {
        position: {
          x: 0,
          y: 1000,
        },
      },
    ],
  };

  return {
    teams: [playerTeam, aiTeam],
  };
})();
