import { Vector2, difference } from "./game/common/Vector";
import { map as mapStream, Stream } from "./Stream";

// ========== Types
export type MouseEventKind =
  | "mousedown"
  | "mouseup"
  | "click"
  | "mousemove"
  | "wheel";

export const enum MouseButton {
  Left = 1,
  Middle = 2,
  Right = 4,
}

// ========== Helpers
export const mouseEventPosition = (event: MouseEvent): Vector2 => ({
  x: event.clientX,
  y: event.clientY,
});

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

export const mouseEvent =
  (kind: MouseEventKind): Stream<MouseEvent> =>
  (emit) => {
    const listener = (e: MouseEvent) => {
      emit(e);
    };

    window.addEventListener(kind, listener);

    return () => window.removeEventListener(kind, listener);
  };

export const mousePosition = (_: MouseEventKind): Stream<Vector2> =>
  mapStream((e) => ({ x: e.clientX, y: e.clientY }), mouseEvent("mousemove"));

export const mouseDownPosition = mousePosition("mousedown");

export const mouseDelta: Stream<Vector2> = (emit) => {
  let last: null | Vector2 = null;

  const cancelMouseMove = mouseEvent("mousemove")((e) => {
    const position = mouseEventPosition(e);

    if (!(e.buttons & MouseButton.Left)) {
      last = null;
      return;
    }

    if (last !== null) {
      emit(difference(position, last));
    }

    last = position;
  });

  return cancelMouseMove;
};

/*
export const mouseDelta: Stream<Vector2> = filter(
  futureMerge(1, mouseEvent("mousemove"), (past, current) => {
    if (!(past.buttons & MouseButton.Left)) return null;
    if (!(current.buttons & MouseButton.Left)) return null;

    return {
      x: current.x - past.x,
      y: current.y - past.y,
    };
  }),
  (b) => b !== null
) as Stream<Vector2>;
*/

/**
 * Stream capturing scrolls on the window
 */
export const wheel = mouseEvent("wheel") as Stream<WheelEvent>;
