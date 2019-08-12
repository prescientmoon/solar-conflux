import { passwordEncryption } from '../types/passwordEncryption'
import { HttpError } from '../../network/classes/HttpError'

/**
 * Comparesa apssword with it's hash
 *
 * @param hash The hash of the password
 * @param password The actual password
 * @param encryption The encription of the password
 */
export const checkPassword = (
    hash: string,
    password: string,
    encryption: passwordEncryption = 'plain'
) => {
    if (encryption === 'plain') {
        return hash === password
    } else {
        throw new HttpError(400, `Encription ${encryption} doesn't exist`)
    }
}
