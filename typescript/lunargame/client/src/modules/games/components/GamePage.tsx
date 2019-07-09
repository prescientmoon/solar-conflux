import React, { UIEvent } from 'react'
import './GamePage.scss'
import { useObservable } from 'rxjs-hooks'
import { GameInfiniteList } from '../constants'
import { GameListItem } from './GameListItem'

export const Games = () => {
    useObservable(() => GameInfiniteList.refresh)

    const ready = useObservable(() => GameInfiniteList.ready)

    if (!ready) {
        return <h1>Loading...</h1>
    }

    const handleScroll = (e: UIEvent<HTMLDivElement>) => {
        const element = e.target as HTMLDivElement

        if (
            element.scrollHeight - element.scrollTop <
            element.clientHeight + 50
        ) {
            GameInfiniteList.loadChunk()
            e.preventDefault()
        }
    }

    return (
        <div onScroll={handleScroll} className="gameListContainer">
            <div className="gameListGrid">
                {GameInfiniteList.data.map(game => (
                    <GameListItem key={game.id} game={game} />
                ))}
            </div>
        </div>
    )
}
