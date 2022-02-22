import { Game } from "../../core/src/game/Game";
import { layers } from "../../core/src/game/State";
import * as Stream from "../../core/src/Stream";
import * as Array from "../../core/src/Array";
import { Canvas } from "./components/Canvas";
import { Stack } from "./components/Stack";

const [contextStreams, contextEmitters] = Array.splitTuples(
  layers.map(() => Stream.create<CanvasRenderingContext2D>())
);

const contexts = Stream.sequence(contextStreams);

const game = new Game(contexts as any);
game.initRenderer();
game.initUpdater();

export function App() {
  return (
    <Stack className="game__game--stack">
      {contextEmitters.map((emit, id) => {
        return (
          <Canvas
            key={id}
            className="stack__stack--layer"
            emitRef={(canvas) => {
              if (id === layers.length - 1) emit(canvas as any);
              else emit(canvas.getContext("2d", {})!);
            }}
          ></Canvas>
        );
      })}
    </Stack>
  );
}
