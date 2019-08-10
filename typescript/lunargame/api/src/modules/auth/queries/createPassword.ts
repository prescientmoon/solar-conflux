import { DbManager } from '../../db/classes/DbManager'
import { Password } from '../types/Password'

const { connection } = new DbManager()

export const createPassword = (
    password: string,
    uid: string
): Promise<Password> => {
    return connection
        .from('user-password')
        .insert<Password>({
            secure: true,
            uid,
            value: password
        })
        .returning('*')
}
