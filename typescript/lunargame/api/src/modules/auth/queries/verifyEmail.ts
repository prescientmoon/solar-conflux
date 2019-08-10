import { DbManager } from '../../db/classes/DbManager'

const { connection } = new DbManager()

export const verifyAccount = (token: string) => {
    return connection
        .from('account')
        .update('verified', true)
        .where('verificationToken', token)
        .returning('verified')
}
