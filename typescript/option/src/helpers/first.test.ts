import { constantly } from '@thi.ng/compose'
import { expect } from 'chai'
import { someX } from '../../test/constants'
import { None, Option } from '../types'
import { first } from './first'

describe('The first helper', () => {
    it('should return the first Some if there is any', () => {
        // act
        const head = first([someX, None])
        const middle = first([None, someX, None])
        const tail = first([None, someX])

        // assert
        expect(head).to.equal(someX)
        expect(middle).to.equal(someX)
        expect(tail).to.equal(someX)
    })

    it("should return None if there isn't any Some", () => {
        // arrange
        const array: Option<unknown>[] = Array(50)
            .fill(1)
            .map(constantly(None))

        // act
        const result = first(array)

        // assert
        expect(result).to.equal(None)
    })
})
