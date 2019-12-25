import { expect } from 'chai'
import { optionify } from './optionify'
import { fromNullable } from './fromNullable'

describe('The optionify helper', () => {
    it('should create a function which returns an option instead of a nullable', () => {
        // arrange
        const func = (a: number, b: number) => (a > b ? a + b : null)

        // act
        const result = optionify(func)

        // assert
        expect(result(1, 2)).to.equal(fromNullable(func(1, 2)))
        expect(result(2, 1)).to.equal(fromNullable(func(2, 1)))
    })
})
