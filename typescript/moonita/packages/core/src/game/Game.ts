import { Effect, Stream } from "../Stream";
import {
  mouseDelta,
  mouseEventPosition,
  screenSizes,
  wheel,
} from "../WebStreams";
import * as Path from "./common/Path";
import { loadPixiTextures, TextureId } from "./assets";
import { defaultFlags, Flag } from "./common/Flags";
import * as V from "./common/Vector";
import { baseSize, basicMap, basicMapPathA } from "./Map";
import {
  createRenderingComponents,
  createRenderingQueries,
  createSimulationComponents,
  createSimulationQueries,
  getCamera,
  getScreenTransform,
  LayerId,
  layers,
  State,
  StateKind,
} from "./State";
import {
  addSprite,
  createBoid,
  markEntityCreation,
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
} from "./systems/renderMap";
import { syncPixiTransforms } from "./systems/renderTextures";
import { applyCameraObject } from "./systems/renderWithTransform";
import * as Camera from "./common/Camera";
import { renderDebugBoidData, simulateBoids } from "./systems/boids";
import { rotateAfterVelocity } from "./systems/rotateAfterVelocity";
import { limitSpeeds } from "./systems/limitSpeeds";
import { QuadTree, QuadTreeSettings } from "../QuadTree";
import { AABB } from "./common/AABB";
import { updateBoidQuadTree } from "./systems/boidQuadTree";
import { FlexibleTypedArray } from "../FlexibleTypedArray";
import { settings } from "./common/Settings";
import { TickScheduler } from "../TickScheduler";
import { handleGameAction } from "./systems/handleGameAction";
import * as PIXI from "pixi.js";
import { identityTransform } from "./common/Transform";
import { ECS } from "wolf-ecs";
import * as GameAction from "./GameAction";
import {
  bubbleSpark,
  bubbleSparkProjectile,
  doubleSpell,
  exampleWand,
  speedUp,
  VanillaCardId,
  VanillaProjectileId,
} from "./wand";
import { spawnWand } from "./systems/interpretWands";

export class Game {
  private state: State | null = null;
  private cancelers: Effect<void>[] = [];

  public constructor(
    canvasElements: Stream<
      [debugCanvas: HTMLCanvasElement, pixiCanvas: HTMLCanvasElement]
    >
  ) {
    const cancelCanvasSubsciptions = canvasElements(
      async ([debugCanvas, pixiCanvas]) => {
        if (this.state === null) {
          const ecs = new ECS(5000, false);
          const flags = defaultFlags;
          const components = {
            ...createSimulationComponents(ecs, flags),
            ...createRenderingComponents(ecs, flags),
          };

          const queries = {
            ...createSimulationQueries(ecs, components),
            ...createRenderingQueries(ecs, components),
          };

          const debugContext = debugCanvas.getContext("2d");

          if (!debugContext) throw new Error(`Couldn't create canvas context!`);

          debugContext.imageSmoothingEnabled = false;

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

          const quadTreeSettings: QuadTreeSettings = {
            transforms: components.transform,
            maxNodes: 20,
            retriveInto: new FlexibleTypedArray(settings.maxBoids, Uint16Array),
            entityMovementBuffer: new FlexibleTypedArray(
              settings.maxBoids,
              Uint16Array
            ),
          };

          const stage = new PIXI.Container();
          const cameraContainer = new PIXI.Container();
          const pixiTextures = await loadPixiTextures();

          stage.name = "Root";
          cameraContainer.name = "Camera";

          for (let i = 0; i < layers.length; i++) {
            const layer = new PIXI.Container();
            layer.name = `Layer ${i}`;
            cameraContainer.addChild(layer);
          }

          const g = new PIXI.Graphics();
          g.beginFill(0xaa00ff, 1);
          g.drawRect(-10, -10, 20, 20);

          cameraContainer.addChild(g);
          stage.addChild(cameraContainer);

          const cameraId = ecs.createEntity();
          const screenId = ecs.createEntity();

          ecs.addComponent(cameraId, components.transform);
          ecs.addComponent(cameraId, components.pixiObject);

          ecs.addComponent(screenId, components.transform);
          ecs.addComponent(screenId, components.pixiObject);

          components.transform[cameraId] = identityTransform();
          components.transform[screenId] = identityTransform();
          components.transform[screenId].scale.y *= -1;

          components.pixiObject.ref[cameraId] = cameraContainer;
          components.pixiObject.ref[screenId] = stage;
          components.pixiObject.scaleBySpriteDimenssions[cameraId] = 0;
          components.pixiObject.scaleBySpriteDimenssions[screenId] = 0;

          this.state = {
            kind: StateKind.Full,
            context: debugContext,
            pixiRenderer: PIXI.autoDetectRenderer({
              view: pixiCanvas,
              backgroundAlpha: 0,
            }),
            pixiStage: stage,
            pixiTextures,
            tickScheduler: new TickScheduler(),
            components,
            queries,
            ecs,
            tick: 0,
            selectedEntity: null,
            map: basicMap,
            paths: [basicMapPathA, Path.flip(basicMapPathA)],
            camera: cameraId,
            screenTransform: screenId,
            flags,
            bounds,
            structures: {
              boidQuadTrees: [
                new QuadTree(bounds, quadTreeSettings),
                new QuadTree(bounds, quadTreeSettings),
              ],
            },

            // Wand related props
            wands: {
              0: exampleWand,
            },

            cards: {
              [VanillaCardId.BubbleSpark]: bubbleSpark,
              [VanillaCardId.DoubleSpell]: doubleSpell,
              [VanillaCardId.SpeedUp]: speedUp,
            },

            projectileBlueprints: {
              [VanillaProjectileId.BubbleSpark]: bubbleSparkProjectile,
            },
          };

          for (const team of basicMap.teams) {
            const eid = ecs.createEntity();
            const transform = identityTransform();

            ecs.addComponent(eid, components.transform);
            components.transform[eid] = transform;

            V.cloneInto(transform.position, team.base);
            V.scaleMut(transform.scale, transform.scale, baseSize);
            transform.rotation = team.base.rotation;

            addSprite(
              this.state,
              eid,
              LayerId.BuildingLayer,
              TextureId.YellowBase
            );
          }

          if (this.state.flags[Flag.DebugGlobalState])
            (globalThis as any).state = this.state;

          this.resizeContext({ x: window.innerWidth, y: window.innerHeight });

          spawnWand(this.state, 0);

          if (this.state.flags[Flag.SpawnDebugBulletEmitter]) {
            const eid = ecs.createEntity();

            markEntityCreation(this.state, eid);

            ecs.addComponent(eid, components.transform);
            ecs.addComponent(eid, components.angularVelocity);

            const transform = identityTransform();

            this.state.components.transform[eid] = transform;

            transform.position.x = 40;
            transform.position.y = 100;
            transform.scale.x = 80;
            transform.scale.y = 80;

            addSprite(
              this.state,
              eid,
              LayerId.BuildingLayer,
              TextureId.BulletSpawner
            );

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

                createBoid(
                  this.state,
                  V.add(V.random2dInsideOriginSquare(-100, 100), {
                    x: p,
                    y: p,
                  }),
                  team
                );
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

            this.resizeContext(size);
          });

          this.cancelers.push(cancelWindowSizes);
        } else throw new Error("Oof, gotta handle this thing now???");
      }
    );

    this.cancelers.push(cancelCanvasSubsciptions);

    this.setupMouseDeltaHandler();
  }

  private setupMouseDeltaHandler() {
    this.cancelers.push(
      mouseDelta((delta) => {
        if (this.state === null) return;

        const camera = this.state.components.transform[this.state.camera];

        Camera.toLocalScaleMut(getScreenTransform(this.state), delta);
        Camera.translateGlobalCoordinatesMut(camera, delta);
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
          getScreenTransform(this.state),
          clientPosition
        );

        Camera.scaleAroundGlobalPointMut(
          this.state.components.transform[this.state.camera],
          inWorldCoordinates,
          scalingVec
        );
      })
    );
  }

  public toLocalCoordinates(vec: V.Vector2) {
    if (!this.state) return vec;

    const inScreenSpace = Camera.toLocalCoordinates(
      getScreenTransform(this.state),
      vec
    );

    return Camera.toLocalCoordinates(getCamera(this.state), inScreenSpace);
  }

  public dispose() {
    for (const canceler of this.cancelers) {
      canceler();
    }

    this.cancelers = [];
  }

  private resizeContext(size: V.Vector2) {
    if (!this.state) return;

    const context = this.state.context;
    context.canvas.width = size.x;
    context.canvas.height = size.y;

    this.state.pixiRenderer.resize(size.x, size.y);

    const screen = getScreenTransform(this.state);

    V.scaleMut(screen.position, size, 1 / 2);
  }

  public render() {
    if (!this.state) return;

    // Reset accumulated transforms
    this.state.context.resetTransform();
    this.state.context.clearRect(0, 0, 10000, 10000);

    syncPixiTransforms(this.state);

    // Apply base transforms
    applyCameraObject(this.state.context, getScreenTransform(this.state));
    applyCameraObject(this.state.context, getCamera(this.state));

    renderDebugQuadTrees(this.state);
    renderDebugArrows(this.state);
    renderDebugPaths(this.state);
    renderDebugBounds(this.state);
    renderDebugBoidData(this.state);

    this.state.pixiRenderer.render(this.state.pixiStage);
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
