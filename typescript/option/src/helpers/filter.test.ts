import { expect } from 'chai'
import { constantly } from '@thi.ng/compose'
import { filter } from './filter'
import { None, Some } from '../types'
import { someX } from '../../test/constants'

describe('The filter helper', () => {
    describe('When the predicate returns true', () => {
        const predicate = constantly(true)

        it('should return None when given None', () => {
            // act
            const result = filter(predicate, None)

            // assert
            expect(result).to.equal(None)
        })

        it('should return Some(x) when given Some(x)', () => {
            // act
            const result = filter(predicate, someX)

            // assert
            expect(result).to.equal(someX)
        })
    })

    describe('When the predicate returns false', () => {
        const predicate = constantly(false)

        it('should return None when given Some', () => {
            // act
            const result = filter(predicate, someX)

            // assert
            expect(result).to.equal(None)
        })

        it('should return None when given None', () => {
            // act
            const result = filter(predicate, None)

            // assert
            expect(result).to.equal(None)
        })
    })
})
