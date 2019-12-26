import { expect } from 'chai'
import { values } from './values'
import { Some, None } from '../types'

describe('The values helper', () => {
    it('should ignore all None values', () => {
        // arrange
        const items = Array(50)
            .fill(1)
            .map((_, i) => (i % 2 ? Some(i) : None))

        // act
        const result = values(items)

        // assert
        expect(result).to.not.contain(None)
        expect(result, "ensure it didn't clear everything").to.not.be.empty
    })
})
