import { Game } from "../../core/src/game/Game";
import * as Stream from "../../core/src/Stream";
import { Canvas } from "./components/Canvas";

const [context, emitContext] = Stream.create<CanvasRenderingContext2D>();

const game = new Game(context);
game.initRenderer();
game.initUpdater();

export function App() {
  return (
    <Canvas
      emitRef={(canvas) => emitContext(canvas.getContext("2d")!)}
    ></Canvas>
  );
}
