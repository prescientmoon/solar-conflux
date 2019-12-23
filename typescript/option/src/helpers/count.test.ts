import { expect } from 'chai'
import { Some, None } from '../types'
import { count } from './count'
import { x } from '../../test/constants'

describe('The count helper', () => {
    it('should return 1 when given Some', () => {
        // act
        const result = count(Some(x))

        // assert
        expect(result).to.equal(1)
    })

    it('should return 0 when given None', () => {
        // act
        const result = count(None)

        // assert
        expect(result).to.equal(0)
    })
})
