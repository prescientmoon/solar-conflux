export const enum Axis {
  Negative = -1,
  Zero = 0,
  Positive = 1,
}

export type Direction = [Axis, Axis];

export interface Note {
  stamp: number;
  orientation: Direction;
}

export interface Chart {
  path: Array<Note>;
  song: HTMLAudioElement;
}
