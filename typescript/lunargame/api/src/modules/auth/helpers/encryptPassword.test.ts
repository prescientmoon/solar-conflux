import { internet } from 'faker'
import { encryptPassword } from './encryptPassword'
import { compare } from 'bcryptjs'

describe('The encryptPassword helper', () => {
    test("should return the same password if the method is 'plain'", async () => {
        const password = internet.password()
        const hash = await encryptPassword(password, 'plain')

        expect(hash).toBe(password)
    })

    test("should return a mactching hash if the method is 'bcrypt'", async () => {
        const password = internet.password()

        // the amount of rounds is small because this is just a test
        const hash = await encryptPassword(password, 'bcrypt', 3)
        const match = await compare(password, hash)

        expect(match).toBe(true)
    })
})
