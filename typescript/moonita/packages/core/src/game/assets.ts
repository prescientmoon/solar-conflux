import * as twgl from "twgl.js";

// ========== Asset imports
import textureBlueBullet from "/public/assets/blue_bullet.svg";
import textureYellowBase from "/public/assets/yellow_base.svg";
import textureBulletSpawner from "/public/assets/bullet_spawner.svg";
import texturePurpleBoid from "/public/assets/purple_boid.svg";
import textureOrangeBoid from "/public/assets/orange_boid.svg";
import spriteVertexShader from "/packages/core/src/game/shaders/sprite.vert";
import spriteFragmentShader from "/packages/core/src/game/shaders/sprite.frag";

export const enum TextureId {
  BlueBullet,
  YellowBase,
  BulletSpawner,
  PurpleBoid,
  OrangeBoid,
}

export const enum ShaderId {
  SpriteShader,
}

const assetPaths: Record<TextureId, string> = {
  [TextureId.BlueBullet]: textureBlueBullet,
  [TextureId.YellowBase]: textureYellowBase,
  [TextureId.BulletSpawner]: textureBulletSpawner,
  [TextureId.PurpleBoid]: texturePurpleBoid,
  [TextureId.OrangeBoid]: textureOrangeBoid,
};

const shaderSources: Record<ShaderId, [string, string]> = {
  [ShaderId.SpriteShader]: [spriteVertexShader, spriteFragmentShader],
};

// Array version of assetPaths
const assetPathArray: ReadonlyArray<string> = Array.from({
  ...assetPaths,
  length: Object.values(assetPaths).length,
});

// Array version of assetPaths
const shaderSourceArray: ReadonlyArray<[string, string]> = Array.from({
  ...shaderSources,
  length: Object.values(shaderSources).length,
});

export interface Texture {
  image: HTMLImageElement;
  inherentRotation: number;
}

// TODO: return promise for better load-time handling
// (eg: displaying a spinner)
export function createGpuAssets(
  gl: WebGL2RenderingContext
): ReadonlyArray<WebGLTexture> {
  return assetPathArray.map((url) => {
    const texture = gl.createTexture();
    if (texture === null) throw new Error(`Failed to create texture`);

    twgl.loadTextureFromUrl(gl, texture, {
      src: url,
    });

    // return new Promise((resolve) => {
    //   image.addEventListener(
    //     "load",
    //     () => {
    //       resolve(null);
    //     },
    //     { once: true }
    //   );
    // });

    return texture;
  });
}

export function createGpuPrograms(
  gl: WebGL2RenderingContext
): ReadonlyArray<WebGLProgram> {
  return shaderSourceArray.map(([vertex, fragment]) => {
    const program = twgl.createProgramFromSources(gl, [vertex, fragment]);

    return program;
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
