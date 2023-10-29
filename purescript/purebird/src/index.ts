// @ts-ignore purescript code
import { main } from "./Main.purs";

console.log("hello world");

const canvas = document.getElementById("canvas")! as HTMLCanvasElement;
const context = canvas.getContext("2d");

let lastTick = performance.now();

const loop = (effect: (n: number) => () => void) => () => {
  requestAnimationFrame(() => {
    const now = performance.now();
    const delta = now - lastTick;
    lastTick = now;

    effect(delta)();
    loop(effect)();
  });
};

main({ context, loop })();
