import { expect } from 'chai'
import { bind } from './bind'
import { Some, None } from '../types'
import { constantly } from '@thi.ng/compose'

describe('The bind helper', () => {
    it('should return none for any callback when given None', () => {
        // act
        const result = bind(Some, None)

        // assert
        expect(result).to.equal(None)
    })

    describe('When given Some', () => {
        it('should return None if the callback returns None', () => {
            // act
            const result = bind(constantly(None), Some(3))

            // assert
            expect(result).to.equal(None)
        })

        it('should return Some if the callback returns Some', () => {
            // act
            const result = bind(x => Some(x + 1), Some(3))

            // assert
            expect(result).to.equal(Some(4))
        })
    })
})
