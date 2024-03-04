import { expect } from 'chai'
import { combine } from './combine'
import { someX } from '../../test/constants'
import { None } from '../types'
import { isSome } from './isSome'

describe('The combine helepr', () => {
    it('should return None if the iterable contains any Nones', () => {
        // act
        const result = combine([someX, someX, None, someX, someX])

        // assert
        expect(result).to.equal(None)
    })

    it("should return Some when the iterable doesn't contain any None", () => {
        // arrange
        const items = Array(50).fill(someX)

        // act
        const result = combine(items)

        // act
        expect(isSome(result)).to.be.true
    })
})
