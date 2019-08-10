import { Account } from '../types/Account'
import { DbManager } from '../../db/classes/DbManager'

const db = new DbManager()

export function getUserByUid(uid: string): Promise<Account | null> {
    return db.connection
        .from('account')
        .select('*')
        .where('uid', uid)
        .first()
}
