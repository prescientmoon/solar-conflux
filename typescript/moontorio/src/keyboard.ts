import { EventEmitter } from "./utils/emitter";

export type KeyboardEvents = Record<string, KeyboardEvent>;

export interface KeyboardState {
  pressed: Set<string>;
  dispose(): void;
  emitter: EventEmitter<KeyboardEvents>;
}

export const pressedKeys = (): KeyboardState => {
  const pressed = new Set<string>();
  const emitter = new EventEmitter<KeyboardEvents>();

  const onKeyDown = (e: KeyboardEvent) => {
    emitter.emit(e.key.toLowerCase(), e);
    pressed.add(e.key);
  };

  const onKeyUp = (e: KeyboardEvent) => pressed.delete(e.key);

  window.addEventListener("keydown", onKeyDown);
  window.addEventListener("keyup", onKeyUp);

  return {
    pressed,
    dispose: () => {
      window.removeEventListener("keydown", onKeyDown);
      window.removeEventListener("keyup", onKeyUp);
    },
    emitter,
  };
};
