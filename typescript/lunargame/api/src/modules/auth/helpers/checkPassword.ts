import { Account } from '../types/Account'
import { Password } from '../types/Password'
import { compare } from 'bcryptjs'

export const checkPassword = async (
    account: Account & Password,
    password: string
) => {
    if (
        (account.secure && (await compare(password, account.value))) || // prod
        (!account.secure && account.value === password) // dev
    ) {
        return true
    }

    return false
}
