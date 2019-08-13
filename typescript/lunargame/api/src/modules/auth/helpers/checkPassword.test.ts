import { checkPassword } from './checkPassword'
import { passwordEncryption } from '../types/passwordEncryption'
import { hash, genSalt } from 'bcryptjs'

describe('The checkPassword helper', () => {
    const pass = 'this is a test password'

    test("should throw an error if the encryption method doesn't exist", async () => {
        // arrange
        const check = checkPassword(pass, pass, '12212' as passwordEncryption)

        // assert
        await expect(check).rejects.toThrow()
    })

    describe("The 'plain' encryption", () => {
        test('should return true if the password is correct', async () => {
            // act
            const check = await checkPassword(pass, pass, 'plain')

            // assert
            expect(check).toBe(true)
        })

        test('shoud return false if the password is wrong', async () => {
            // act
            const check = await checkPassword(pass, pass + 'something', 'plain')

            // assert
            expect(check).toBe(false)
        })
    })

    describe("The 'bcrypt' encryption", () => {
        let passwordHash: string

        beforeEach(async () => {
            const salt = await genSalt(3)

            passwordHash = await hash(pass, salt)
        })

        test('should return true if the password is correct', async () => {
            // act
            const check = await checkPassword(passwordHash, pass, 'bcrypt')

            // assert
            expect(check).toBe(true)
        })

        test('shoud return false if the password is wrong', async () => {
            // act
            const check = await checkPassword(passwordHash, pass + 'something', 'bcrypt')

            // assert
            expect(check).toBe(false)
        })
    })
})
