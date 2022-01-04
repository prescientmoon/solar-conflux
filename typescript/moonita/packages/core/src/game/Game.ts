import { ECS } from "wolf-ecs";
import { Effect, Stream } from "../Stream";
import { screenSizes } from "../WebStreams";
import { assets } from "./assets";
import { identityTransform } from "./common/Transform";
import { basicMap } from "./Map";
import { createComponents, createQueries, State } from "./State";
import { markEntityCreation } from "./systems/createEntity";
import { moveEntities } from "./systems/moveEntities";
import {
  renderBullets,
  renderBulletSpawners,
} from "./systems/renderBulletSpawner";
import { renderTextures } from "./systems/renderTextures";
import { applyTransformObject } from "./systems/renderWithTransform";
import { despawnBullets, spawnBullets } from "./systems/spawnBullet";

const ups = 30;

export class Game {
  private state: State | null = null;
  private cancelers: Effect<void>[] = [];

  public constructor(context: Stream<CanvasRenderingContext2D>) {
    const cancelContexts = context((ctx) => {
      if (this.state === null) {
        const ecs = new ECS(1000, false);
        const components = createComponents(ecs);
        const queries = createQueries(ecs, components);

        this.state = {
          ctx,
          components,
          queries,
          ecs,
          tick: 0,
          assets,
          map: basicMap,
          camera: identityTransform(),
          screenTransform: identityTransform(),
        };

        this.resizeContext();

        const id = ecs.createEntity();

        markEntityCreation(this.state, id);

        ecs.addComponent(id, components.bulletEmitter);
        ecs.addComponent(id, components.transform);

        this.state.components.transform.position.x[id] = 40;
        this.state.components.transform.position.y[id] = 100;
        this.state.components.transform.rotation[id] = 0;
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
      } else this.state.ctx = ctx;
    });

    this.cancelers.push(cancelContexts);
  }

  public dispose() {
    for (const canceler of this.cancelers) {
      canceler();
    }

    this.cancelers = [];
  }

  private resizeContext() {
    if (!this.state) return;

    this.state.ctx.canvas.width = window.innerWidth;
    this.state.ctx.canvas.height = window.innerHeight;
  }

  public render() {
    if (!this.state) return;

    this.state.ctx.clearRect(0, 0, 10000, 10000);

    // Apply base transforms
    applyTransformObject(this.state, this.state.screenTransform);
    applyTransformObject(this.state, this.state.camera);

    renderBulletSpawners(this.state);

    //    renderBullets(this.state);
    renderTextures(this.state);

    // Reset accumulated transforms
    this.state.ctx.resetTransform();

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
