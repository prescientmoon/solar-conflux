import * as GameAction from "./GameAction";
import { ECS } from "wolf-ecs";
import { Effect, Stream } from "../Stream";
import {
  mouseDelta,
  mouseEventPosition,
  screenSizes,
  wheel,
} from "../WebStreams";
import * as Path from "./common/Path";
import { assets, TextureId } from "./assets";
import { defaultFlags, Flag } from "./common/Flags";
import * as V from "./common/Vector";
import { basicMap, basicMapPathA } from "./Map";
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
  renderDebugQuadTrees,
  renderMap,
} from "./systems/renderMap";
import { renderTextures } from "./systems/renderTextures";
import { applyGlobalCameraObject } from "./systems/renderWithTransform";
import * as Camera from "./common/Camera";
import { renderDebugBoidData, simulateBoids } from "./systems/boids";
import { rotateAfterVelocity } from "./systems/rotateAfterVelocity";
import { limitSpeeds } from "./systems/limitSpeeds";
import { randomBetween, TAU } from "../math";
import { QuadTree } from "../QuadTree";
import { AABB } from "./common/AABB";
import { updateBoidQuadTree } from "./systems/boidQuadTree";
import { FlexibleTypedArray } from "../FlexibleTypedArray";
import { settings } from "./common/Settings";
import { TickScheduler } from "../TickScheduler";
import { handleGameAction } from "./systems/handleGameAction";

export class Game {
  private state: State | null = null;
  private cancelers: Effect<void>[] = [];

  public constructor(contexts: Stream<Array<CanvasRenderingContext2D>>) {
    const cancelContexts = contexts((contexts) => {
      if (this.state === null) {
        const ecs = new ECS(5000, false);
        const flags = defaultFlags;
        const components = createComponents(ecs, flags);
        const queries = createQueries(ecs, components);

        contexts.forEach((ctx) => {
          ctx.imageSmoothingEnabled = false;
        });

        // TODO: extract bounds related functionality in it's own module
        const bounds: AABB = {
          position: {
            x: -1200,
            y: -1200,
          },
          size: {
            x: 2400,
            y: 2400,
          },
        };

        const quadTreeSettings = {
          positions: components.transform.position,
          maxNodes: 20,
          retriveInto: new FlexibleTypedArray(settings.maxBoids, Uint16Array),
          entityMovementBuffer: new FlexibleTypedArray(
            settings.maxBoids,
            Uint16Array
          ),
        };

        this.state = {
          contexts: contexts,
          tickScheduler: new TickScheduler(),
          components,
          queries,
          ecs,
          tick: 0,
          selectedEntity: null,
          assets,
          map: basicMap,
          paths: [basicMapPathA, Path.flip(basicMapPathA)],
          camera: Camera.identityCamera(),
          screenTransform: Camera.defaultScreenTransform(),
          flags,
          bounds,
          structures: {
            boidQuadTrees: [
              new QuadTree(bounds, quadTreeSettings),
              new QuadTree(bounds, quadTreeSettings),
            ],
          },
        };

        if (this.state.flags[Flag.DebugGlobalState])
          (globalThis as any).state = this.state;

        this.resizeContext();

        if (this.state.flags[Flag.SpawnDebugBulletEmitter]) {
          const eid = ecs.createEntity();

          markEntityCreation(this.state, eid);

          ecs.addComponent(eid, components.bulletEmitter);
          ecs.addComponent(eid, components.transform);
          ecs.addComponent(eid, components.texture);
          ecs.addComponent(eid, components.angularVelocity);

          this.state.components.transform.position.x[eid] = 40;
          this.state.components.transform.position.y[eid] = 100;
          this.state.components.transform.rotation[eid] = 0;
          this.state.components.transform.scale.x[eid] = 1;
          this.state.components.transform.scale.y[eid] = 1;
          this.state.components.texture.width[eid] = 80;
          this.state.components.texture.height[eid] = 80;
          this.state.components.texture.textureId[eid] =
            TextureId.BulletSpawner;
          this.state.components.texture.layer[eid] = LayerId.BuildingLayer;
          this.state.components.bulletEmitter.frequency[eid] = 5;
          this.state.components.angularVelocity[eid] = 0.1;

          this.state.tickScheduler.schedule(
            10,
            GameAction.handleBulletSpawner(eid),
            5
          );
        }

        if (this.state.flags[Flag.SpawnDebugBoids]) {
          for (let team = 0; team < this.state.map.teams.length; team++) {
            for (
              let i = 0;
              i < settings.maxBoids / this.state.map.teams.length;
              i++
            ) {
              const p = team === 0 ? -900 : 900;
              const eid = createBoid(
                this.state,
                V.add(V.random2dInsideOriginSquare(-100, 100), { x: p, y: p }),
                team
              );

              const angle = 0; // randomBetween(0, TAU);

              // setVelocity(this.state, eid, Math.cos(angle), Math.sin(angle));
              setVelocity(this.state, eid, 0, 0);
            }
          }

          this.state.selectedEntity = {
            id: 90,
            isPathFollower: true,
          };
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

    this.state.screenTransform = Camera.defaultScreenTransform();
  }

  public render() {
    if (!this.state) return;

    // Reset accumulated transforms
    for (const context of this.state.contexts) {
      context.resetTransform();
    }

    for (const context of this.state.contexts) {
      if (context === this.state.contexts[LayerId.Unclearable]) continue;
      context.clearRect(0, 0, 10000, 10000);
    }

    // Apply base transforms
    applyGlobalCameraObject(this.state, this.state.screenTransform);
    applyGlobalCameraObject(this.state, this.state.camera);

    renderMap(this.state);
    renderTextures(this.state);

    renderDebugQuadTrees(this.state);
    renderDebugArrows(this.state);
    renderDebugPaths(this.state);
    renderDebugBounds(this.state);
    renderDebugBoidData(this.state);
  }

  public update() {
    if (!this.state) return;

    this.state.tick++;

    const tasks = this.state.tickScheduler.getTasks(this.state.tick);

    for (let i = 0; i < tasks.length; i++)
      handleGameAction(this.state, tasks[i]);

    this.state.tickScheduler.handleTick(this.state.tick);

    simulateBoids(this.state);
    updateVelocities(this.state);
    limitSpeeds(this.state);
    moveEntities(this.state);

    rotateEntities(this.state);
    rotateAfterVelocity(this.state);

    for (let team = 0; team < this.state.map.teams.length; team++) {
      updateBoidQuadTree(this.state, team);
    }
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
      if (!(window as any).pauseGame) this.update();
    };

    const id = setInterval(loop, 1000 / settings.ups);

    return () => clearInterval(id);
  }
}
