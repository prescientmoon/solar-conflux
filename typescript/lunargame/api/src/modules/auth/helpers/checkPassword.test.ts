import { checkPassword } from './checkPassword'
import { passwordEncryption } from '../types/passwordEncryption'

describe('The checkPassword helper', () => {
    const pass = 'this is a test password'

    test("should throw an error if the encryption method doesn't exist", () => {
        expect(() => {
            checkPassword(pass, pass, '12212' as passwordEncryption)
        }).toThrow()
    })

    test('should return true if the password matches the hash and the encryption = plain', () => {
        expect(checkPassword(pass, pass, 'plain')).toBe(true)
    })

    test('shoud return false if the password is wrong and the encryption = plain', () => {
        expect(checkPassword(pass, pass + 'something', 'plain')).toBe(false)
    })
})
