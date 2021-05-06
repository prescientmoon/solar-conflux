import { GameState } from "../../gameState";
import { canvas } from "../../index";

export const renderFixedText = (
    fontSize: number,
    fontStyle: string,
    row: number,
    column: number,
    state: GameState,
    text: string,
    color?: string | CanvasPattern | CanvasGradient
) => {
    // font size independed on the camera zoom
    const FontSize = fontSize / state.camera.scale;
    // set the font and the fill style
    state.ctx.font = `${FontSize}px ${fontStyle}`;
    state.ctx.fillStyle = color || "black";
    // render the text at the desired location
    state.ctx.fillText(
        text,
        canvas.width / (canvas.width / FontSize) -
            state.camera.translation[0] +
            (column - 0.5) * FontSize,
        canvas.height / (canvas.height / FontSize) -
            state.camera.translation[1] +
            (row + 0.5) * FontSize
    );
};
