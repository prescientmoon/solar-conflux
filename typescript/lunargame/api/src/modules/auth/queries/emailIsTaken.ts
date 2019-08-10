import { DbManager } from '../../db/classes/DbManager'

const { connection } = new DbManager()

export const emailIsTaken = async (email: string) => {
    return await connection
        .from('account')
        .select('email')
        .where('email', email)
        .first()
}
