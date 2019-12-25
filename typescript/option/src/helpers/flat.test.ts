import { expect } from 'chai'
import { flat } from './flat'
import { None, Some } from '../types'
import { someX } from '../../test/constants'

describe('The flat helper', () => {
    it('should return None when given None', () => {
        // act
        const result = flat(None)

        // assert
        expect(result).to.equal(None)
    })

    it('should return the inner Some(x) when given Some(Some(x))', () => {
        // arrange
        const value = Some(someX)

        // act
        const result = flat(value)

        // assert
        expect(result).to.equal(someX)
    })
})
