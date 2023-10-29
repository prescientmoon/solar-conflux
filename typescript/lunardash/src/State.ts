import type { Chart } from './Chart';

export const enum NoteJudgement {
  Missed,
  Far,
  Perfect,
}

export const enum NoteTiming {
  Early,
  Late,
}

export interface NoteState {
  judgement: NoteJudgement;
  timing: NoteTiming;
}

export interface State {
  song: HTMLAudioElement;
  currentNote: number;
  notes: NoteState[];
  readonly chart: Chart;
  readonly settings: Settings;
}

export interface Settings {
  offset: number;
  judgments: {
    perfect: number;
    far: number;
  };
  scrollSpeed: number;
  implicitScrollSpeed: number;
  windowCenter: number;
  noteSize: number;
}
