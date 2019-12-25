import { constantly } from '@thi.ng/compose'
import { expect } from 'chai'
import { spy } from 'sinon'
import { x, alwaysSomeX } from '../../test/constants'
import { None, Some } from '../types'
import { oneOf } from './oneOf'

const alwaysSome = <T>(v: T) => constantly(Some(v))

describe('The oneOf helper', () => {
    it('should return None on an empty array', () => {
        // act
        const result = oneOf(x, [])

        // assert
        expect(result).to.equal(None)
    })

    it('should return the result of the first function which evaluates to Some', () => {
        // arrange
        const alwaysNone = constantly(None)

        // act
        const head = oneOf(x, [alwaysSome('head'), alwaysNone])
        const middle = oneOf(x, [alwaysNone, alwaysSome('middle'), alwaysNone])
        const tail = oneOf(x, [alwaysNone, alwaysSome('tail')])

        // assert
        expect(head).to.equal(Some('head'))
        expect(middle).to.equal(Some('middle'))
        expect(tail).to.equal(Some('tail'))
    })

    it('should not evaluate any more functions after it found the result', () => {
        // arrange
        const func = spy(alwaysSomeX)

        // act
        oneOf(x, [alwaysSomeX, func])

        // assert
        expect(func.called).to.be.false
    })

    it('should pass the provided input to the functions', () => {
        // arrange
        const func = spy(alwaysSomeX)

        // act
        oneOf(x, [func])

        // assert
        expect(func.calledWith(x)).to.be.true
    })
})
