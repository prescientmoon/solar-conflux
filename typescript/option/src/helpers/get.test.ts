import { expect } from 'chai'
import { get } from './get'
import { None } from '../types'
import { someX, x } from '../../test/constants'

describe('The get helper', () => {
    it('should throw when given None', () => {
        // act
        const callable = () => get(None)

        // assert
        expect(callable).to.throw()
    })

    it('should return the innter value when given Some', () => {
        // act
        const result = get(someX)

        // assert
        expect(result).to.equal(x)
    })
})
