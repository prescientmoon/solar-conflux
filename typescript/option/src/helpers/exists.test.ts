import { expect } from 'chai'
import { exists } from './exists'
import { constantly } from '@thi.ng/compose'
import { None, Some } from '../types'
import { x } from '../../test/constants'

describe('The exists helper', () => {
    it('should return false when given None', () => {
        // act
        const result = exists(constantly(true), None)

        // assert
        expect(result).to.equal(false)
    })

    describe('When given Some', () => {
        it('should return true if the callback returns true', () => {
            // act
            const result = exists(constantly(true), Some(x))

            // assert
            expect(result).to.equal(true)
        })

        it('should return false if the callback returns false', () => {
            // act
            const result = exists(constantly(false), Some(x))

            // assert
            expect(result).to.equal(false)
        })
    })
})
