export interface KeyboardState {
  pressed: Set<string>;
  dispose(): void;
}

export const pressedKeys = (): KeyboardState => {
  const pressed = new Set<string>();

  const onKeyDown = (e: KeyboardEvent) => pressed.add(e.key);
  const onKeyUp = (e: KeyboardEvent) => pressed.delete(e.key);

  window.addEventListener("keydown", onKeyDown);
  window.addEventListener("keyup", onKeyUp);

  return {
    pressed,
    dispose: () => {
      window.removeEventListener("keydown", onKeyDown);
      window.removeEventListener("keyup", onKeyUp);
    },
  };
};
