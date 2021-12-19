// ========== Asset imports
import textureBlueBullet from "/public/assets/blue_bullet.svg";

export const enum TextureId {
  BlueBullet,
}

const assetPaths: Record<TextureId, string> = {
  [TextureId.BlueBullet]: textureBlueBullet,
};

export interface Texture {
  image: HTMLImageElement;
  inherentRotation: number;
}

export const assets: ReadonlyArray<Texture> = Object.entries(assetPaths).reduce(
  (previous, current) => {
    const image = new Image(100, 100);
    image.src = current[1];

    previous[current[0] as unknown as number] = {
      image,
      inherentRotation: Math.PI / 2,
    };

    return previous;
  },
  [] as Array<Texture>
);
