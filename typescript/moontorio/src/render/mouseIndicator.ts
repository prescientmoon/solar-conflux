import { settings } from "../constants";
import { GameState } from "../gameState";

export const renderIndicator = (
    tilePosition: [number, number],
    state: GameState
) => {
    state.ctx.beginPath();
    state.ctx.rect(
        tilePosition[0] * settings.tileSize,
        tilePosition[1] * settings.tileSize,
        settings.tileSize,
        settings.tileSize
    );
    state.ctx.stroke();
};
