import { expect } from 'chai'
import { None } from '../types'
import { alwaysX, x, someX } from '../../test/constants'
import { constantly } from '@thi.ng/compose'
import { spy } from 'sinon'
import { unpack } from './unpack'

describe('The unpack helper', () => {
    describe('When given None', () => {
        it('should return the default when given None', () => {
            // act
            const result = unpack(constantly(0), constantly(1), None)

            // assert
            expect(result).to.equal(0)
        })

        it('should call the lazy default', () => {
            // arrange
            const func = spy(alwaysX)

            // act
            unpack(func, alwaysX, None)

            // assert
            expect(func.called).to.be.true
        })
    })

    describe('When given Some', () => {
        it('should return the return of the mapper', () => {
            // act
            const result = unpack(constantly(0), constantly(1), someX)

            // assert
            expect(result).to.equal(1)
        })

        it('should not call the lazy default', () => {
            // arrange
            const func = spy(alwaysX)

            // act
            unpack(func, alwaysX, someX)

            // assert
            expect(func.called).to.be.false
        })

        it('should pass the inner value to the mapper', () => {
            // arrange
            const mapper = spy(alwaysX)

            // act
            unpack(alwaysX, mapper, someX)

            // assert
            expect(mapper.calledWith(x)).to.be.true
        })
    })
})
