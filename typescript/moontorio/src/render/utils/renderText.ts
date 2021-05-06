import { GameState } from "../../gameState";
import { canvas } from "../../index";

export const renderFixedText = (settings: {
    fontSize?: number;
    fontStyle?: string;
    row: number;
    column: number;
    state: GameState;
    text: string;
    color?: string | CanvasPattern | CanvasGradient;
}) => {
    // font size independed on the camera zoom
    const FontSize = settings.fontSize || 20 / settings.state.camera.scale;
    // set the font and the fill style
    settings.state.ctx.font = `${FontSize}px ${settings.fontStyle || "Arial"}`;
    settings.state.ctx.fillStyle = settings.color || "black";
    // render the text at the desired location
    settings.state.ctx.fillText(
        settings.text,
        canvas.width / (canvas.width / FontSize) -
            settings.state.camera.translation[0] +
            (settings.column - 0.5) * FontSize,
        canvas.height / (canvas.height / FontSize) -
            settings.state.camera.translation[1] +
            (settings.row + 0.5) * FontSize
    );
};
