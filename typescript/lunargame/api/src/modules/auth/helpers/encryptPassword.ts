import { passwordEncryption } from '../types/passwordEncryption'
import { genSalt, hash } from 'bcryptjs'

/**
 * Encypts a string
 *
 * @param password The password to encrypt
 * @param method The method to encrypt the password with
 * @param rounds The salting rounds (for bcrypt only)
 */
export const encryptPassword = async (
    password: string,
    method: passwordEncryption,
    rounds = 10
) => {
    if (method === 'bcrypt') {
        const salt = await genSalt(rounds)
        const result = await hash(password, salt)

        return result
    } else {
        return password
    }
}
