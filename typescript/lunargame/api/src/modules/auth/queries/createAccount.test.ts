/* eslint-disable @typescript-eslint/explicit-function-return-type */
import { name, random, internet } from 'faker'
import { createAccount } from './createAccount'
import { connection } from '../../db/connection'
import { SignupBody } from '../schemas/SignupBody'

describe('The createAccount query', () => {
    test('should return the id of the account and add it to the db', async () => {
        const email = internet.email()
        const username = name.firstName()
        const password = random.alphaNumeric(10)

        const result = await createAccount({
            email,
            name: username,
            password,
            passwordEncryption: 'plain'
        })

        const account = await connection
            .from('account')
            .select<Required<SignupBody>>(['email', 'name', 'password'])
            .where({
                id: result
            })
            .first()

        expect(account.name).toBe(username)
        expect(account.email).toBe(email)
        expect(account.password).toBe(password)
    })
})
