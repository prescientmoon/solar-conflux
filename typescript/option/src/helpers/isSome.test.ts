import { expect } from 'chai'
import { None } from '../types'
import { someX } from '../../test/constants'
import { isSome } from './isSome'

describe('The isSome helper', () => {
    it('should return true when given Some', () => {
        // act
        const result = isSome(someX)

        // assert
        expect(result).to.equal(true)
    })

    it('should return false when given None', () => {
        // act
        const result = isSome(None)

        // assert
        expect(result).to.equal(false)
    })
})
