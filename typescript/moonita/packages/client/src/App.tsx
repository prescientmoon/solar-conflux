import { Game } from "../../core/src/game/Game";
import { layers } from "../../core/src/game/State";
import * as Stream from "../../core/src/Stream";
import * as Array from "../../core/src/Array";
import { Canvas } from "./components/Canvas";
import { Stack } from "./components/Stack";

const [canvasStreams, canvasEmitters] = Array.splitTuples(
  layers.map(() => Stream.create<HTMLCanvasElement>())
);

const canvases = Stream.sequence(canvasStreams);
const game = new Game(
  canvases as Stream.Stream<[HTMLCanvasElement, HTMLCanvasElement]>
);

game.initRenderer();
game.initUpdater();

export function App() {
  return (
    <Stack className="game__game--stack">
      {canvasEmitters.map((emit, id) => {
        return (
          <Canvas
            key={id}
            className="stack__stack--layer"
            emitRef={emit}
          ></Canvas>
        );
      })}
    </Stack>
  );
}
