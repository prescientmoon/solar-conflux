import React, { CSSProperties } from 'react'
import './GameListItem.scss'
import { GameChunkElementData } from '../../network/types/GameChunkElementData'

export const GameListItem = (props: { game: GameChunkElementData }) => {
    const styles: CSSProperties = {
        backgroundImage: `url(${props.game.thumbail})`
    }

    return (
        <div className="gameListItemContainer">
            <div className="gameListItem" style={styles} />
        </div>
    )
}
