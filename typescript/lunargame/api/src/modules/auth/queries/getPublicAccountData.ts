import { DbManager } from '../../db/classes/DbManager'
import { publicAccountFields } from '../constants'

const { connection } = new DbManager()

export const getPublicAccountData = (field: string, value: string) => {
    return connection
        .from('account')
        .select(...publicAccountFields)
        .where(field, value)
        .first()
}
