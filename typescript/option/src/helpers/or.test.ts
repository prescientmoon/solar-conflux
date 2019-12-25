import { expect } from 'chai'
import { or } from './or'
import { someX } from '../../test/constants'
import { None } from '../types'

describe('The or helper', () => {
    describe('When the first argument is None', () => {
        it('should return the second argument', () => {
            // act
            const orSome = or(None, someX)
            const orNone = or(None, None)

            // assert
            expect(orSome).to.equal(someX)
            expect(orNone).to.equal(None)
        })
    })

    it("should return the first argument when it's not None", () => {
        // act
        const result = or(someX, None)

        // assert
        expect(result).to.equal(someX)
    })
})
