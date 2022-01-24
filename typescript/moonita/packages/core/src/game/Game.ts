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
import {
  createBoid,
  markEntityCreation,
  setVelocity,
} from "./systems/createEntity";
import { renderDebugArrows } from "./systems/debugArrows";
import {
  moveEntities,
  rotateEntities,
  updateVelocities,
} from "./systems/moveEntities";
import {
  renderDebugBounds,
  renderDebugPaths,
  renderMap,
} from "./systems/renderMap";
import { renderTextures } from "./systems/renderTextures";
import { applyGlobalCameraObject } from "./systems/renderWithTransform";
import { despawnBullets, spawnBullets } from "./systems/spawnBullet";
import * as Camera from "./common/Camera";
import { simulateBoids } from "./systems/boids";
import { rotateAfterVelocity } from "./systems/rotateAfterVelocity";
import { limitSpeeds } from "./systems/limitSpeeds";
import { randomBetween, TAU } from "../math";
import Quadtree from "@timohausmann/quadtree-js";

const ups = 30;

export class Game {
  private state: State | null = null;
  private cancelers: Effect<void>[] = [];

  public constructor(contexts: Stream<Array<CanvasRenderingContext2D>>) {
    const cancelContexts = contexts((contexts) => {
      if (this.state === null) {
        const ecs = new ECS(3000, false);
        const components = createComponents(ecs);
        const queries = createQueries(ecs, components);

        // TODO: extract bounds related functionality in it's own module
        const bounds: [V.Vector2, V.Vector2] = [
          {
            x: -1200,
            y: -1200,
          },
          {
            y: 1200,
            x: 1200,
          },
        ];

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
          thrusterConfigurations: [],
          bounds,
          structures: {
            boidQuadTree: new Quadtree({
              x: bounds[0].x,
              y: bounds[0].y,
              width: bounds[1].x - bounds[0].x,
              height: bounds[1].y - bounds[0].y,
            }),
          },
        };

        if (this.state.flags[Flag.DebugGlobalState])
          (globalThis as any).state = this.state;

        this.resizeContext();

        if (this.state.flags[Flag.SpawnDebugBulletEmitter]) {
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
        }

        if (this.state.flags[Flag.SpawnDebugBoids]) {
          for (let i = 0; i < 100; i++) {
            const eid = createBoid(
              this.state,
              V.random2dInsideOriginSquare(-1000, 1000)
            );

            const angle = randomBetween(0, TAU);

            setVelocity(this.state, eid, Math.cos(angle), Math.sin(angle));
          }
        }

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
    renderTextures(this.state);

    renderDebugArrows(this.state);
    renderDebugPaths(this.state);
    renderDebugBounds(this.state);

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

    simulateBoids(this.state);
    updateVelocities(this.state);
    limitSpeeds(this.state);
    moveEntities(this.state);

    rotateEntities(this.state);
    rotateAfterVelocity(this.state);
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
