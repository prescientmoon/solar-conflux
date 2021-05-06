import { GameState } from "../gameState";
import { renderFixedText } from "./utils/renderText";

const getFPS = (state: GameState) => {
    return Math.floor((state.tick / (state.time / 1000)) * 100) / 100;
};

const msToTimestamp = (ms: number) => {
    return new Date(ms).toISOString().substr(11, 8);
};

export const renderDebugger = async (state: GameState) => {
    renderFixedText(20, "Arial", 0, 0, state, `Game Frame: ${state.tick}`);
    renderFixedText(
        20,
        "Arial",
        1,
        0,
        state,
        `Game Time: ${msToTimestamp(state.time)}`
    );

    renderFixedText(20, "Arial", 2, 0, state, `FPS: ${getFPS(state)}`, "blue");
    renderFixedText(
        20,
        "Arial",
        3,
        0,
        state,
        `X: ${Math.floor(state.player.position[0] * 100) / 100} Y: ${
            Math.floor(state.player.position[1] * 100) / 100
        }`,
        "blue"
    );
};
