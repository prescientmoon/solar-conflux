export const getCursor = (
  cursor: string | string[],
  mouseState: number,
  button = 1
) => {
  const clicked = button < 0 ? mouseState : (mouseState >> button) & 1

  if (typeof cursor === "string") {
    return cursor
  } else if (cursor.length === 1) {
    return cursor
  } else if (cursor.length === 2) {
    return cursor[Number(clicked)]
  } else {
    throw new Error(
      `Cannot resolve cursor:\nMouseState: ${mouseState}\nCursor: ${cursor}\nButton: ${button}`
    )
  }
}
