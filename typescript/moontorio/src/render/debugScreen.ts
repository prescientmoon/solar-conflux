import { GameState } from "../gameState";
import { renderFixedText } from "./utils/renderText";

const getFPS = (state: GameState) => {
    return Math.floor((state.tick / (performance.now() / 1000)) * 100) / 100;
};

export const renderDebugger = async (state: GameState) => {
    renderFixedText(
        20,
        "Arial",
        0,
        0,
        state,
        `Game Tick: ${state.tick}`,
        "blue"
    );
    renderFixedText(
        20,
        "Arial",
        2,
        0,
        state,
        `Game Time: ${state.time}`,
        "blue"
    );

    renderFixedText(20, "Arial", 1, 0, state, `FPS: ${getFPS(state)}`, "red");
};
