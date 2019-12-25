import { expect } from 'chai'
import { isNone } from './isNone'
import { None } from '../types'
import { someX } from '../../test/constants'

describe('The isNone helper', () => {
    it('should return false when given Some', () => {
        // act
        const result = isNone(someX)

        // assert
        expect(result).to.equal(false)
    })

    it('should return true when given None', () => {
        // act
        const result = isNone(None)

        // assert
        expect(result).to.equal(true)
    })
})
