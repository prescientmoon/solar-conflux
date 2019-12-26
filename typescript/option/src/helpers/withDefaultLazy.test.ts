import { expect } from 'chai'
import { withDefaultLazy } from './withDefaultLazy'
import { None, Some } from '../types'
import { alwaysX, x, someX } from '../../test/constants'
import { constantly } from '@thi.ng/compose'
import { spy } from 'sinon'

describe('The withDefaultLazy helper', () => {
    describe('When given None', () => {
        it('should return the default when given None', () => {
            // act
            const result = withDefaultLazy(alwaysX, None)

            // assert
            expect(result).to.equal(x)
        })

        it('should call the lazy default', () => {
            // arrange
            const func = spy(constantly(x))

            // act
            withDefaultLazy(func, None)

            // assert
            expect(func.called).to.be.true
        })
    })

    describe('When given Some', () => {
        it('should return the inner value', () => {
            // act
            const result = withDefaultLazy(constantly(0), Some(1))

            // assert
            expect(result).to.equal(1)
        })

        it('should not call the lazy default', () => {
            // arrange
            const func = spy(constantly(x))

            // act
            withDefaultLazy(func, someX)

            // assert
            expect(func.called).to.be.false
        })
    })
})
