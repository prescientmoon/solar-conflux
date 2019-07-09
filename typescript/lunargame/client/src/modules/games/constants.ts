import { InfiniteList } from '../../common/dom/classes/InfiniteList'
import { GameChunkElementData } from '../network/types/GameChunkElementData'

export const GameInfiniteList = new InfiniteList<GameChunkElementData>(
    'gameList',
    {
        pageSize: 5,
        urls: {
            chunk: 'game/chunk',
            count: 'game/count'
        },
        initialLoads: 6
    }
)
