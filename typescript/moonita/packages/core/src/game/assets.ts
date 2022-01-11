// ========== Asset imports
import textureBlueBullet from "/public/assets/blue_bullet.svg";
import textureYellowBase from "/public/assets/yellow_base.svg";
import textureBulletSpawner from "/public/assets/bullet_spawner.svg";

export const enum TextureId {
  BlueBullet,
  YellowBase,
  BulletSpawner,
}

const assetPaths: Record<TextureId, string> = {
  [TextureId.BlueBullet]: textureBlueBullet,
  [TextureId.YellowBase]: textureYellowBase,
  [TextureId.BulletSpawner]: textureBulletSpawner,
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
