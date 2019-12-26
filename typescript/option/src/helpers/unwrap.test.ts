import { constantly, identity } from '@thi.ng/compose'
import { expect } from 'chai'
import { spy } from 'sinon'
import { someX, x } from '../../test/constants'
import { None } from '../types'
import { unwrap } from './unwrap'

describe('The unwrap helper', () => {
    it('should return the default when given None', () => {
        // act
        const result = unwrap(0, constantly(1), None)

        // assert
        expect(result).to.equal(0)
    })

    describe('When given Some', () => {
        it('should return the result of the mapper ', () => {
            // act
            const result = unwrap(0, constantly(1), someX)

            // assert
            expect(result).to.equal(1)
        })

        it('should pass the inner value to the mapper', () => {
            // arrange
            const mapper = spy(identity)

            // act
            unwrap(0, mapper, someX)

            // assert
            expect(mapper.calledWith(x)).to.be.true
        })
    })
})
