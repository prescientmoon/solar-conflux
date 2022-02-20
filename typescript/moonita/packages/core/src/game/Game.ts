import * as GameAction from "./GameAction";
import * as twgl from "twgl.js";
import { ECS } from "wolf-ecs";
import { Effect, Stream } from "../Stream";
import {
  mouseDelta,
  mouseEventPosition,
  screenSizes,
  wheel,
} from "../WebStreams";
import * as Path from "./common/Path";
import {
  assets,
  createGpuAssets,
  createGpuPrograms,
  TextureId,
} from "./assets";
import { defaultFlags, Flag } from "./common/Flags";
import * as V from "./common/Vector";
import { basicMap, basicMapPathA } from "./Map";
import {
  createComponents,
  createQueries,
  LayerId,
  layers,
  State,
} from "./State";
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
import { renderTextures, renderWebglSprites } from "./systems/renderTextures";
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
import { createTextures } from "twgl.js";
import { SpriteRenderer } from "./webgl/SpriteRenderer";
import { mat2d, mat3, vec2 } from "gl-matrix";

export class Game {
  private state: State | null = null;
  private cancelers: Effect<void>[] = [];

  public constructor(
    contexts: Stream<
      [...Array<CanvasRenderingContext2D>, WebGL2RenderingContext]
    >
  ) {
    const cancelContexts = contexts((contexts) => {
      if (this.state === null) {
        const ecs = new ECS(5000, false);
        const flags = defaultFlags;
        const components = createComponents(ecs, flags);
        const queries = createQueries(ecs, components);

        contexts.forEach((ctx) => {
          if (ctx instanceof CanvasRenderingContext2D)
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

        const gl = contexts.pop() as WebGL2RenderingContext;
        const programs = createGpuPrograms(gl);

        const projectionMatrix = mat3.create();
        const worldMatrix = mat3.create();

        this.state = {
          contexts: contexts as any,
          gl,
          projectionMatrix,
          worldMatrix,
          webglRenderers: {
            spriteRenderer: new SpriteRenderer(
              gl,
              projectionMatrix,
              worldMatrix,
              layers.length,
              programs
            ),
          },
          tickScheduler: new TickScheduler(),
          components,
          queries,
          ecs,
          tick: 0,
          selectedEntity: null,
          assets,
          textures: createGpuAssets(gl),
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

        this.regenerateProjectionMatrix();

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
      } else {
        const gl = contexts.pop();

        this.state.contexts = contexts as any;
        this.state.gl = gl as any;
      }
    });

    this.cancelers.push(cancelContexts);

    this.setupMouseDeltaHandler();
  }

  private regenerateProjectionMatrix() {
    if (!this.state) return;

    const gl = this.state.gl;
    mat3.identity(this.state.projectionMatrix);
    mat3.scale(this.state.projectionMatrix, this.state.projectionMatrix, [
      2 / gl.canvas.width,
      2 / gl.canvas.height,
    ]);
  }

  private setupMouseDeltaHandler() {
    this.cancelers.push(
      mouseDelta((delta) => {
        if (this.state === null) return;

        Camera.toLocalScaleMut(this.state.screenTransform, delta);
        Camera.translateGlobalCoordinatesMut(this.state.camera, delta);

        const camera = this.state.worldMatrix;
        const deltaVec = vec2.fromValues(delta.x, delta.y);
        const originVec = vec2.create();
        const icamera = mat3.invert(mat3.create(), camera);

        vec2.transformMat3(deltaVec, deltaVec, icamera);
        vec2.transformMat3(originVec, originVec, icamera);
        vec2.subtract(deltaVec, deltaVec, originVec);
        mat3.translate(camera, camera, deltaVec);
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

        const camera = this.state.worldMatrix;
        const deltaVec = vec2.fromValues(delta, delta);
        const gl = this.state.gl;

        const fixedClientPosition = vec2.fromValues(
          clientPosition.x - gl.canvas.width / 2,
          -clientPosition.y + gl.canvas.height / 2
        );

        // TODO: abstract this away
        mat3.multiply(
          camera,
          [
            deltaVec[0],
            0,
            0,
            0,
            deltaVec[1],
            0,
            fixedClientPosition[0] * (1 - deltaVec[0]),
            fixedClientPosition[1] * (1 - deltaVec[1]),
            1,
          ],
          camera
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

    twgl.resizeCanvasToDisplaySize(this.state.gl.canvas);

    this.regenerateProjectionMatrix();
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

    const gl = this.state.gl;

    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    // gl.enable(gl.DEPTH_TEST);
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    // Apply base transforms
    applyGlobalCameraObject(this.state, this.state.screenTransform);
    applyGlobalCameraObject(this.state, this.state.camera);

    renderMap(this.state);
    renderTextures(this.state);
    renderWebglSprites(this.state);

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
