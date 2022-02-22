import * as PIXI from "pixi.js";

// ========== Asset imports
import textureBlueBullet from "/public/assets/blue_bullet.svg";
import textureYellowBase from "/public/assets/yellow_base.svg";
import textureBulletSpawner from "/public/assets/bullet_spawner.svg";
import texturePurpleBoid from "/public/assets/purple_boid.svg";
import textureOrangeBoid from "/public/assets/orange_boid.svg";

export const enum TextureId {
  BlueBullet,
  YellowBase,
  BulletSpawner,
  PurpleBoid,
  OrangeBoid,
}

const assetPaths: Record<TextureId, string> = {
  [TextureId.BlueBullet]: textureBlueBullet,
  [TextureId.YellowBase]: textureYellowBase,
  [TextureId.BulletSpawner]: textureBulletSpawner,
  [TextureId.PurpleBoid]: texturePurpleBoid,
  [TextureId.OrangeBoid]: textureOrangeBoid,
};

// Array version of assetPaths
const assetPathArray: ReadonlyArray<string> = Array.from({
  ...assetPaths,
  length: Object.values(assetPaths).length,
});

export interface Texture {
  image: HTMLImageElement;
  inherentRotation: number;
}

export function loadPixiTextures(): Promise<PIXI.Texture[]> {
  return new Promise((resolve) => {
    const loader = new PIXI.Loader();
    for (const [id_, path] of Object.entries(assetPaths)) {
      loader.add(id_, path);
    }

    loader.load((loader, resources) => {
      resolve(
        Array.from<PIXI.LoaderResource>({
          ...resources,
          length: assetPathArray.length,
        }).map((r: PIXI.LoaderResource) => r.texture!)
      );
    });
  });
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

// ========== Constants
export const boidTextureByTeam = [TextureId.PurpleBoid, TextureId.OrangeBoid];
