import { components, createGroup, Env } from "../../ecs";
import { renderAnimation } from "./animation";

const group = createGroup(
  [components.groundAnimation, components.position, components.direction],
  `Ground animating`
);

export const update = (env: Env) => {
  group.forEach((entity) => {
    renderAnimation(
      env,
      entity.groundAnimation,
      entity.position,
      entity.direction.direction
    );
  });
};
