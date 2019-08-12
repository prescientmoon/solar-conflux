import { getPasswordByEmail } from './getPasswordByEmail'
import { mockAccounts } from '../../../../test/seeds/01_create-account'
import { connection } from '../../db/connection'

describe('The getPasswordByName query', () => {
    test('should return the correct password & encryption for a mock account', async () => {
        await connection.seed.run()

        for (const account of mockAccounts) {
            const result = await getPasswordByEmail(account.email)

            expect(result.password).toBe(account.password)
            expect(result.passwordEncryption).toBe(account.passwordEncryption)
        }
    })
})
