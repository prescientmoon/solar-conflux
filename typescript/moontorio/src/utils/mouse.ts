import { settings } from "../constants";
import { GameState } from "../gameState";

// Gets the mouse position and returns the tile numbers
export const getHoveredTile = (
    position: [number, number]
): [number, number] => {
    return [
        Math.floor(position[0] / settings.tileSize),
        Math.floor(position[1] / settings.tileSize),
    ];
};

export const fixMousePosition = (state: GameState) => {
    state.mouse.position = [
        state.mouse.position[0] - state.camera.translation[0],
        state.mouse.position[1] - state.camera.translation[1],
    ];
};
