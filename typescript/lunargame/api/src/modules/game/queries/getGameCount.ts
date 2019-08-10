import { DbManager } from '../../db/classes/DbManager'
import { CountData } from '../../../common/rest/types/CountData'

const { connection } = new DbManager()

export const getGameCount = async () => {
    const { count } = await connection
        .from('game')
        .count('id')
        .first<CountData>()

    return Number(count)
}
