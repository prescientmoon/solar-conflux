import { expect } from 'chai'
import { withDefault } from './withDefault'
import { x } from '../../test/constants'
import { None, Some } from '../types'

describe('The withDefault helper', () => {
    it('should return the default when given None', () => {
        // act
        const result = withDefault(x, None)

        // assert
        expect(result).to.equal(x)
    })

    it('should return x when given Some(x)', () => {
        // act
        const result = withDefault(0, Some(1))

        // assert
        expect(result).to.equal(1)
    })
})
