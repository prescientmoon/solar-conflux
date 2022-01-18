import { ECS } from "wolf-ecs";
import { Effect, Stream } from "../Stream";
import {
  mouseDelta,
  mouseEventPosition,
  screenSizes,
  wheel,
} from "../WebStreams";
import { assets, TextureId } from "./assets";
import { defaultFlags, Flag } from "./common/Flags";
import * as V from "./common/Vector";
import { basicMap } from "./Map";
import { createComponents, createQueries, LayerId, State } from "./State";
import { markEntityCreation } from "./systems/createEntity";
import { renderDebugArrows } from "./systems/debugArrows";
import { moveEntities, rotateEntities } from "./systems/moveEntities";
import { renderDebugPaths, renderMap } from "./systems/renderMap";
import { renderTextures } from "./systems/renderTextures";
import { applyGlobalCameraObject } from "./systems/renderWithTransform";
import { despawnBullets, spawnBullets } from "./systems/spawnBullet";
import * as Camera from "./common/Camera";

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
          camera: Camera.identityCamera(),
          screenTransform: Camera.flipYMut(Camera.identityCamera()),
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
        ecs.addComponent(id, components.angularVelocity);

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
        this.state.components.angularVelocity[id] = 0.1;

        // Listen to resize events
        const cancelWindowSizes = screenSizes((size) => {
          if (!this.state) return;

          size.x /= 2;
          size.y /= 2;

          this.resizeContext();
          Camera.translateGlobalCoordinatesMut(
            this.state.screenTransform,
            size
          );
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

        Camera.toLocalScaleMut(this.state.screenTransform, delta);
        Camera.translateGlobalCoordinatesMut(this.state.camera, delta);
      })
    );

    this.cancelers.push(
      wheel((e) => {
        if (!this.state) return;
        if (e.deltaY === 0) return;

        const scrollConstant = 0.8;
        const delta = e.deltaY > 0 ? scrollConstant : 1 / scrollConstant;
        const scalingVec = { x: delta, y: delta };

        const clientPosition: V.Vector2 = mouseEventPosition(e);
        const inWorldCoordinates = Camera.toLocalCoordinates(
          this.state.screenTransform,
          clientPosition
        );

        Camera.scaleAroundGlobalPointMut(
          this.state.camera,
          inWorldCoordinates,
          scalingVec
        );
      })
    );
  }

  public toLocalCoordinates(vec: V.Vector2) {
    if (!this.state) return vec;

    const inScreenSpace = Camera.toLocalCoordinates(
      this.state.screenTransform,
      vec
    );

    return Camera.toLocalCoordinates(this.state.camera, inScreenSpace);
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
    applyGlobalCameraObject(this.state, this.state.screenTransform);
    applyGlobalCameraObject(this.state, this.state.camera);

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
  }

  public update() {
    if (!this.state) return;

    this.state.tick++;

    spawnBullets(this.state);
    despawnBullets(this.state);
    moveEntities(this.state);
    rotateEntities(this.state);
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
