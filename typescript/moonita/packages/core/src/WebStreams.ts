import { Vector2 } from "./game/common/Transform";
import { Stream } from "./Stream";

/**
 * Stream emitting the latest window sizes
 */
export const screenSizes: Stream<Vector2> = (emit) => {
  const listener = () => {
    emit({
      x: window.innerWidth,
      y: window.innerHeight,
    });
  };

  window.addEventListener("resize", listener);

  // kickStart
  listener();

  return () => window.removeEventListener("resize", listener);
};
