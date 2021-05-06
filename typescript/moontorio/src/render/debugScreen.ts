import { GameState } from "../gameState";
import { renderFixedText } from "./utils/renderText";
import { getHoveredTile } from "../utils/mouse";

const getFPS = (state: GameState) => {
    return Math.floor((state.tick / (state.time / 1000)) * 100) / 100;
};

const msToTimestamp = (ms: number) => {
    return new Date(ms).toISOString().substr(11, 8);
};

export const renderDebugger = async (state: GameState) => {
    renderFixedText({
        row: 0,
        column: 0,
        state,
        text: `Game Frame: ${state.tick}`,
    });
    renderFixedText({
        row: 1,
        column: 0,
        state,
        text: `Game Time: ${msToTimestamp(state.time)}`,
    });

    renderFixedText({
        row: 2,
        column: 0,
        state,
        text: `FPS: ${getFPS(state)}`,
        color: "blue",
    });
    renderFixedText({
        row: 3,
        column: 0,
        state,
        text: `X: ${Math.floor(state.player.position[0] * 100) / 100} Y: ${
            Math.floor(state.player.position[1] * 100) / 100
        }`,
        color: "blue",
    });

    renderFixedText({
        row: 4,
        column: 0,
        state,
        text: `Player on tile: ${getHoveredTile(state.player.position)} `,
    });
    renderFixedText({
        row: 5,
        column: 0,
        state,
        text: `Mouse coords: ${Math.floor(
            state.mouse.position[0]
        )},${Math.floor(state.mouse.position[1])} `,
    });
    renderFixedText({
        row: 6,
        column: 0,
        state,
        text: `Mouse on tile: ${getHoveredTile(state.mouse.position)} `,
    });
};
