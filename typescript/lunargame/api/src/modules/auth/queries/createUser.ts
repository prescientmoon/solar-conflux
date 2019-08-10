import { Account } from '../types/Account'
import { DbManager } from '../../db/classes/DbManager'

const { connection } = new DbManager()

const defaultAvatar =
    'https://themango.co/wp-content/uploads/2018/03/Mango-Default-Profile-Pic.png'

export const createUser = async (
    name: string,
    email: string,
    uid: string,
    token: string
) => {
    const data: Account = {
        name,
        email,
        uid,
        avatar: defaultAvatar,
        description: '',
        verified: false,
        verificationToken: token
    }

    return connection
        .from('account')
        .insert(data)
        .returning('*')
}
