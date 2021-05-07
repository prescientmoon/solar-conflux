import { GameState, Tile } from "../gameState";
import { chunkSize } from "../map";
import { tileAt } from "../systems/world";
import { array } from "./iterate";
import { Vec2 } from "./types";

const getBigestLength = (lengths: number[]) => {
    return lengths.sort((a, b) => a - b)[lengths.length - 1];
};

export const allTiles = function* (state: GameState): Generator<[Tile | null]> {
    const BiggestLengthOfChunks = getBigestLength([
        state.map.chunkMap[1][1].length,
        state.map.chunkMap[0][1].length,
        state.map.chunkMap[1][0].length,
        state.map.chunkMap[0][0].length,
    ]);
    // console.log(BiggestLengthOfChunks);

    for (
        let chunkX = -1 * BiggestLengthOfChunks;
        chunkX < BiggestLengthOfChunks;
        chunkX++
    )
        for (
            let chunkY = -1 * BiggestLengthOfChunks;
            chunkY < BiggestLengthOfChunks;
            chunkY++
        ) {
            const chunk =
                state.map.chunkMap[chunkX >= 0 ? 0 : 1][chunkY >= 0 ? 0 : 1]?.[
                    Math.abs(chunkX)
                ]?.[Math.abs(chunkY)];
            if (!chunk) continue;

            if (chunkX >= 0 && chunkY >= 0)
                for (let tileX = 0; tileX < chunkSize; tileX++) {
                    for (let tileY = 0; tileY < chunkSize; tileY++) {
                        yield [chunk[tileX][tileY]];
                    }
                }
            else if (chunkX < 0 && chunkY >= 0)
                for (let tileX = chunkSize - 1; tileX >= 0; tileX--) {
                    for (let tileY = 0; tileY < chunkSize; tileY++) {
                        yield [chunk[tileX][tileY]];
                    }
                }
            else if (chunkX >= 0 && chunkY < 0)
                for (let tileX = 0; tileX < chunkSize; tileX++) {
                    for (let tileY = chunkSize - 1; tileY >= 0; tileY--) {
                        yield [chunk[tileX][tileY]];
                    }
                }
            else if (chunkX < 0 && chunkY < 0)
                for (let tileX = chunkSize - 1; tileX >= 0; tileX--) {
                    for (let tileY = chunkSize - 1; tileY >= 0; tileY--) {
                        yield [chunk[tileX][tileY]];
                    }
                }
        }
};
