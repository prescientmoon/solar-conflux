import { internet } from 'faker'
import { encryptPassword } from './encryptPassword'
import { compare } from 'bcryptjs'

describe('The encryptPassword helper', () => {
    test("should return the same password if the method is 'plain'", async () => {
        // arrange
        const password = internet.password()

        // act
        const hash = await encryptPassword(password, 'plain')

        // assert
        expect(hash).toBe(password)
    })

    test("should return a mactching hash if the method is 'bcrypt'", async () => {
        // arrange
        const password = internet.password()
        const hash = await encryptPassword(password, 'bcrypt', 3)

        // act
        const match = await compare(password, hash)

        // assert
        expect(match).toBe(true)
    })
})
