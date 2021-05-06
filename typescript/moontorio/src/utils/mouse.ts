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
