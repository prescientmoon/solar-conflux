import type { KeyboardState } from "./keyboard";
import type { Player } from "./player";

export interface GameState {
  ctx: CanvasRenderingContext2D;
  camera: {
    translation: [number, number];
    scale: number;
  };
  keyboard: KeyboardState;
  player: Player;
}

type Image = HTMLImageElement;

let imageMap = new Map<string, Image>();

export const loadAsset = (src: string): Image => {
  if (imageMap.has(src)) return imageMap.get(src)!;

  const result = new Image();
  result.src = src;

  result.onload = () => {
    result.height = result.naturalHeight;
    result.width = result.naturalWidth;

    console.log(result);
  };

  imageMap.set(src, result);

  return result;
};
