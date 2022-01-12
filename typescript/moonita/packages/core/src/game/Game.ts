import { ECS } from "wolf-ecs";
import { Effect, Stream } from "../Stream";
import { mouseDelta, screenSizes } from "../WebStreams";
import { assets, TextureId } from "./assets";
import { defaultFlags, Flag } from "./common/Flags";
import { flipYMut, identityTransform } from "./common/Transform";
import { basicMap } from "./Map";
import { createComponents, createQueries, LayerId, State } from "./State";
import { markEntityCreation } from "./systems/createEntity";
import { renderDebugArrows } from "./systems/debugArrows";
import { moveEntities } from "./systems/moveEntities";
import { renderDebugPaths, renderMap } from "./systems/renderMap";
import { renderTextures } from "./systems/renderTextures";
import { applyGlobalTransformObject } from "./systems/renderWithTransform";
import { despawnBullets, spawnBullets } from "./systems/spawnBullet";

const ups = 30;

export class Game {
  private state: State | null = null;
  private cancelers: Effect<void>[] = [];

  public constructor(contexts: Stream<Array<CanvasRenderingContext2D>>) {
    const cancelContexts = contexts((contexts) => {
      if (this.state === null) {
        const ecs = new ECS(1000, false);
        const components = createComponents(ecs);
        const queries = createQueries(ecs, components);

        this.state = {
          contexts: contexts,
          components,
          queries,
          ecs,
          tick: 0,
          assets,
          map: basicMap,
          camera: identityTransform(),
          screenTransform: flipYMut(identityTransform()),
          flags: defaultFlags,
        };

        if (this.state.flags[Flag.DebugGlobalState])
          (globalThis as any).state = this.state;

        this.resizeContext();

        const id = ecs.createEntity();

        markEntityCreation(this.state, id);

        ecs.addComponent(id, components.bulletEmitter);
        ecs.addComponent(id, components.transform);
        ecs.addComponent(id, components.texture);

        this.state.components.transform.position.x[id] = 40;
        this.state.components.transform.position.y[id] = 100;
        this.state.components.transform.rotation[id] = 0;
        this.state.components.transform.scale.x[id] = 1;
        this.state.components.transform.scale.y[id] = 1;
        this.state.components.texture.width[id] = 80;
        this.state.components.texture.height[id] = 80;
        this.state.components.texture.textureId[id] = TextureId.BulletSpawner;
        this.state.components.texture.layer[id] = LayerId.BuildingLayer;
        this.state.components.bulletEmitter.frequency[id] = 5;

        // Listen to resize events
        const cancelWindowSizes = screenSizes((size) => {
          if (!this.state) return;

          size.x /= 2;
          size.y /= 2;

          this.resizeContext();
          this.state.screenTransform.position = size;
        });

        this.cancelers.push(cancelWindowSizes);
      } else this.state.contexts = contexts;
    });

    this.cancelers.push(cancelContexts);

    this.setupMouseDeltaHandler();
  }

  private setupMouseDeltaHandler() {
    this.cancelers.push(
      mouseDelta((delta) => {
        if (this.state === null) return;

        const sx =
          delta.x *
          this.state.camera.scale.x *
          this.state.screenTransform.scale.x;
        const sy =
          delta.y *
          this.state.camera.scale.y *
          this.state.screenTransform.scale.y;

        this.state.camera.position.x += sx;
        this.state.camera.position.y += sy;
      })
    );
  }

  public dispose() {
    for (const canceler of this.cancelers) {
      canceler();
    }

    this.cancelers = [];
  }

  private resizeContext() {
    if (!this.state) return;

    for (const context of this.state.contexts) {
      context.canvas.width = window.innerWidth;
      context.canvas.height = window.innerHeight;
    }
  }

  public render() {
    if (!this.state) return;

    for (const context of this.state.contexts) {
      context.clearRect(0, 0, 10000, 10000);
    }

    // Apply base transforms
    applyGlobalTransformObject(this.state, this.state.screenTransform);
    applyGlobalTransformObject(this.state, this.state.camera);

    renderMap(this.state);
    // renderBulletSpawners(this.state);
    // renderBullets(this.state);
    renderTextures(this.state);

    renderDebugArrows(this.state);
    renderDebugPaths(this.state);

    // Reset accumulated transforms
    for (const context of this.state.contexts) {
      context.resetTransform();
    }

    this.state.camera.rotation += 0.001;
  }

  public update() {
    if (!this.state) return;

    this.state.tick++;

    spawnBullets(this.state);
    despawnBullets(this.state);
    moveEntities(this.state);
  }

  public initRenderer(): Effect<void> {
    let stopped = false;
    const loop = () => {
      this.render();

      if (!stopped) requestAnimationFrame(loop);
    };

    loop();

    return () => (stopped = true);
  }

  public initUpdater(): Effect<void> {
    const loop = () => {
      this.update();
    };

    const id = setInterval(loop, 1000 / ups);

    return () => clearInterval(id);
  }
}
