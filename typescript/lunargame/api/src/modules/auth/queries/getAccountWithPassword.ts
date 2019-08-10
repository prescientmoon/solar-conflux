import { Account } from '../types/Account'
import { Password } from '../types/Password'
import { DbManager } from '../../db/classes/DbManager'
import { publicAccountFields, AccountField } from '../constants'

const db = new DbManager()

export function getUserWithPassword(
    field: string,
    value: string
): Promise<Account & Password | null> {
    return db.connection
        .from('account')
        .innerJoin('user-password', 'account.uid', 'user-password.uid')
        .where(`account.${field}`, value)
        .first()
}
