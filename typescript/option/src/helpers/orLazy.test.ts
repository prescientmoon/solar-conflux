import { expect } from 'chai'
import { constantly } from '@thi.ng/compose'
import { orLazy } from './orLazy'
import { someX } from '../../test/constants'
import { None } from '../types'
import { spy } from 'sinon'

describe('The orLazy helper', () => {
    it("should return the first argument if it's Some", () => {
        // act
        const result = orLazy(someX, constantly(None))

        // asser
        expect(result).to.equal(someX)
    })

    it('should return the return of the second argument if the first is None', () => {
        // act
        const orSome = orLazy(None, constantly(someX))
        const orNone = orLazy(None, constantly(None))

        // assert
        expect(orSome).to.equal(someX)
        expect(orNone).to.equal(None)
    })

    it('should not evaluate the second argument if the first one is Some', () => {
        // arrange
        const func = spy(constantly(someX))

        // act
        orLazy(someX, func)

        // assert
        expect(func.called).to.be.false
    })

    it('should evaluate the second argument if the first one is None', () => {
        // arrange
        const func = spy(constantly(someX))

        // act
        orLazy(None, func)

        // assert
        expect(func.called).to.be.true
    })
})
