import { GameState, Tile } from "../gameState";
import { chunkSize } from "../map";
import { tileAt } from "../systems/world";
import { Vec2 } from "./types";

export const allTiles = function* (
    state: GameState
): Generator<[Tile | null, Vec2]> {
    for (
        let directionalMatrixX = 0;
        directionalMatrixX < state.map.chunkMap.length;
        directionalMatrixX++
    )
        for (
            let directionalMatrixY = 0;
            directionalMatrixY < state.map.chunkMap.length;
            directionalMatrixY++
        )
            for (
                let chunkX = 0;
                chunkX <
                state.map.chunkMap[directionalMatrixX][directionalMatrixY]
                    .length;
                chunkX++
            )
                for (
                    let chunkY = 0;
                    chunkY <
                    state.map.chunkMap[directionalMatrixX][directionalMatrixY]
                        .length;
                    chunkY++
                ) {
                    const chunk =
                        state.map.chunkMap[directionalMatrixX][
                            directionalMatrixY
                        ][chunkX][chunkY];

                    // console.log(tileAt(state, [0, 0]));

                    if (!chunk) continue;

                    for (let tileX = 0; tileX < chunkSize; tileX++) {
                        for (let tileY = 0; tileY < chunkSize; tileY++) {
                            const tile = chunk[tileX][tileY];

                            yield [
                                tile,
                                [
                                    chunkX * chunkSize + tileX,
                                    chunkY * chunkSize + tileY,
                                ],
                            ];
                        }
                    }
                }
};
