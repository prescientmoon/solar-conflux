import { DbManager } from '../../db/classes/DbManager'

const { connection } = new DbManager()

export const getGameChunk = (page: number, pageSize: number) => {
    const offset = page * pageSize

    return connection
        .from('game')
        .select('id', 'avatar', 'thumbail')
        .offset(offset)
        .limit(pageSize)
}
