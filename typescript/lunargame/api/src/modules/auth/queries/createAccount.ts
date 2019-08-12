import { connection } from '../../db/connection'
import { DbAccount } from '../types/Account'

/**
 * Saves a new user into the db
 *
 * @param user The user object to insert
 */
export const createAccount = async (user: DbAccount): Promise<number> => {
    const result = await connection.from('account').insert({
        ...user
    })

    return result[0]
}
