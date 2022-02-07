import { Path, PathId } from "./common/Path";
import { Vector2 } from "./common/Vector";

// ========== Types
export interface PathPoint {
  position: Vector2;
}

export type CreaturePath = Array<PathPoint>;

export interface Team {
  base: Vector2 & { rotation: number };
  hostileTo: number;
  creaturePath: PathId;
}

export interface Map {
  teams: Team[];
}

// ========== Constants
export const basicMapPathA: Path = [
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
];

export const basicMap: Map = (() => {
  const playerTeam: Team = {
    base: {
      x: -1000,
      y: -1000,
      rotation: Math.PI / 4,
    },
    hostileTo: 2,
    creaturePath: 0,
  };

  const aiTeam: Team = {
    base: {
      x: 1000,
      y: 1000,
      rotation: (5 * Math.PI) / 4,
    },
    hostileTo: 1,
    creaturePath: 1,
  };

  return {
    teams: [playerTeam, aiTeam],
  };
})();

export const baseSize = 800;
