import { ECS } from "wolf-ecs";
import { Effect, Stream } from "../Stream";
import { assets } from "./assets";
import { createComponents, createQueries, State } from "./State";
import { markEntityCreation } from "./systems/createEntity";
import { moveEntities } from "./systems/moveEntities";
import {
  renderBullets,
  renderBulletSpawners,
} from "./systems/renderBulletSpawner";
import { renderTextures } from "./systems/renderTextures";
import { despawnBullets, spawnBullets } from "./systems/spawnBullet";

const ups = 30;

export class Game {
  private state: State | null = null;

  public constructor(context: Stream<CanvasRenderingContext2D>) {
    context((ctx) => {
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
        };

        ctx.canvas.width = window.innerWidth;
        ctx.canvas.height = window.innerHeight;

        const id = ecs.createEntity();

        markEntityCreation(this.state, id);

        ecs.addComponent(id, components.bulletEmitter);
        ecs.addComponent(id, components.transform);

        this.state.components.transform.position.x[id] = 40;
        this.state.components.transform.position.y[id] = 100;
        this.state.components.transform.rotation[id] = 0;
        this.state.components.bulletEmitter.frequency[id] = 5;
      } else this.state.ctx = ctx;
    });
  }

  public render() {
    if (!this.state) return;

    this.state.ctx.clearRect(0, 0, 10000, 10000);

    renderBulletSpawners(this.state);
    //    renderBullets(this.state);
    renderTextures(this.state);
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
