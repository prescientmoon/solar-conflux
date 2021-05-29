export const enum MouseButton {
  Left = 1,
  Right = 2,
  Wheel = 4,
  Side1 = 8,
  Side2 = 16,
}

export const isPressed = (target: MouseButton, buttons: number) =>
  buttons & target;
