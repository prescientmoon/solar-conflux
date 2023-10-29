import type { Chart, Note } from './Chart';
import { NoteJudgement, NoteTiming, State } from './State';

const canvas = document.getElementById('canvas')! as HTMLCanvasElement;
const ctx = canvas.getContext('2d')!;

const song = new Audio('/songs/heracles.mp3');

const heracles: Chart = {
  song,
  path: [
    {
      stamp: 1.906999,
      orientation: [0, 1],
    },
    {
      stamp: 3.115963,
      orientation: [0, 1],
    },
    {
      stamp: 4.365792,
      orientation: [0, 1],
    },
    {
      stamp: 5.645609,
      orientation: [0, 1],
    },
    {
      stamp: 6.894808,
      orientation: [0, 1],
    },
    {
      stamp: 8.099023,
      orientation: [0, 1],
    },
    {
      stamp: 9.337156,
      orientation: [0, 1],
    },
  ],
};

let started = false;

const state: State = {
  chart: heracles,
  currentNote: 0,
  notes: [],
  song,
  settings: {
    offset: 0.05,
    judgments: {
      far: 0.1,
      perfect: 0.042,
    },
    scrollSpeed: 4,
    implicitScrollSpeed: 100,
    windowCenter: 100,
    noteSize: 10,
  },
};

const start = () => {
  started = true;

  state.song.play();

  main();
  resize();
};

const resize = () => {
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;
};

const main = () => {
  if (state.song.currentTime > state.chart.path[state.currentNote].stamp) {
    state.currentNote++;
  }

  const time = state.song.currentTime - state.settings.offset;

  while (true) {
    const firstNote = state.chart.path[state.notes.length];
    const delta = time - firstNote.stamp;

    if (delta > state.settings.judgments.far) {
      state.notes.push({
        timing: NoteTiming.Late,
        judgement: NoteJudgement.Missed,
      });
    } else break;
  }

  ctx.clearRect(0, 0, 3000, 3000);

  ctx.fillStyle = 'rgba(0,0,0,0.3)';
  ctx.fillRect(0, 200, window.innerWidth, 100);

  ctx.fillStyle = 'blue';
  ctx.fillRect(
    state.settings.windowCenter - state.settings.noteSize / 2,
    200,
    state.settings.noteSize,
    100,
  );

  ctx.fillStyle = 'rgba(0,0,256,0.3)';
  const farSize =
    state.settings.judgments.far *
    state.settings.scrollSpeed *
    state.settings.implicitScrollSpeed;

  ctx.fillRect(state.settings.windowCenter - farSize, 200, farSize * 2, 100);

  for (let index = 0; index < heracles.path.length; index++) {
    const note = heracles.path[index];
    if (state.notes[index]?.judgement === NoteJudgement.Missed)
      ctx.fillStyle = 'red';
    else if (state.notes[index]?.judgement === NoteJudgement.Far)
      ctx.fillStyle =
        state.notes[index].timing === NoteTiming.Late ? 'orange' : 'blue';
    else if (state.notes[index]?.judgement === NoteJudgement.Perfect)
      ctx.fillStyle = 'green';
    else ctx.fillStyle = 'yellow';

    ctx.fillRect(
      state.settings.windowCenter +
        (note.stamp - song.currentTime) *
          state.settings.scrollSpeed *
          state.settings.implicitScrollSpeed,
      200,
      state.settings.noteSize,
      100,
    );
  }

  requestAnimationFrame(main);
};

canvas.onclick = () => {
  if (!started) return start();

  const time = state.song.currentTime - state.settings.offset;

  while (true) {
    const firstNote = state.chart.path[state.notes.length];

    if (!firstNote) return;

    const delta = time - firstNote.stamp;
    const absoluteDelta = Math.abs(delta);
    const timing = delta < 0 ? NoteTiming.Early : NoteTiming.Late;

    console.log({ timing, delta, firstNote, absoluteDelta, time });

    if (delta > state.settings.judgments.far) {
      state.notes.push({
        timing: NoteTiming.Late,
        judgement: NoteJudgement.Missed,
      });
      continue;
    } else if (absoluteDelta > state.settings.judgments.far) return;
    else
      state.notes.push({
        timing,
        judgement:
          absoluteDelta < state.settings.judgments.perfect
            ? NoteJudgement.Perfect
            : NoteJudgement.Far,
      });

    return;
  }
};
