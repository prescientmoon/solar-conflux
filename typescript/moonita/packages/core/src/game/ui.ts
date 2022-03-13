import * as PIXI from "pixi.js";
import * as E from "wolf-ecs";
import type { Vector2 } from "./common/Vector";
import { SimpleSystem, State } from "./State";

/** Options the createBar function takes for customization purpouses */
export interface BarOptions {
  padding: Vector2;

  /** Total size of the bar *including the padding* */
  size: Vector2;

  backgroundColor: [color: number, alpha: number];
  barColor: [color: number, alpha: number];
}

export function createBar(container: PIXI.Container, options: BarOptions) {
  // Create a container the caller of this function is free to move around
  // (along with everything else in here)
  const barContainer = new PIXI.Container();
  const actualBar = new PIXI.Graphics();
  const barBackground = new PIXI.Graphics();

  const barHeight = options.size.x - 2 * options.padding.x;

  // Custom pivot for ease of scaling (eg: updateBar)
  barContainer.pivot.set(options.size.x / 2, options.size.y / 2);
  actualBar.pivot.set(0, barHeight / 2);

  // Creates the rectangles
  barBackground.beginFill(...options.backgroundColor);
  barBackground.drawRect(0, 0, options.size.x, options.size.y);
  barBackground.endFill();

  actualBar.position.set(options.padding.x, options.padding.y);

  actualBar.beginFill(...options.barColor);
  actualBar.drawRect(
    0,
    barHeight / 2,
    barHeight,
    options.size.y - 2 * options.padding.y
  );
  actualBar.endFill();

  // Add the 2 ui elements to the container we have creted
  barContainer.addChild(barBackground);
  barContainer.addChild(actualBar);

  // Add the ui inside the parent container we were given
  container.addChild(barContainer);

  return barContainer;
}

/** Update the length of the bar.
 * @param bar The bar to update
 * @param value The length of the bar. Can range between 0 (empty) and 255 (full)
 * */
export function updateBar(bar: PIXI.Container, value: number) {
  const transform = bar.children[1];

  transform.scale.x -= (transform.scale.x - value / 255) / 5;
}

/** Updates all the bars in the ecs */
export const updateBarValues = SimpleSystem<State>(
  (components) => E.all<any>(components.uiBar, components.pixiObject),
  (state, eid) => {
    const bar = state.components.pixiObject.ref[eid];

    updateBar(bar, state.components.uiBar[eid]);
  }
);
