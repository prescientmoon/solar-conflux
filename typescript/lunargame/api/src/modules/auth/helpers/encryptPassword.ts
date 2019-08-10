import { genSalt, hash } from 'bcryptjs'

export const encryptPassword = async (saltRounds = 10, password: string) => {
    const salt = await genSalt(saltRounds)
    const passwordHash = await hash(password, salt)

    return passwordHash
}
