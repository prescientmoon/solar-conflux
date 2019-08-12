import { connection } from '../../db/connection'
import { passwordEncryption } from '../types/passwordEncryption'

/**
 * The result of the getPasswordByName query
 */
export interface PasswordByEmailResult {
    password: string
    passwordEncryption: passwordEncryption
    id: number
}

/**
 * Gets the password, passwordEncryption and id of an account from it's email
 *
 * @param email The email of the account
 */
export const getPasswordByEmail = (email: string): Promise<PasswordByEmailResult> => {
    return connection
        .from('account')
        .select('password', 'passwordEncryption', 'id')
        .where({
            email
        })
        .first()
}
