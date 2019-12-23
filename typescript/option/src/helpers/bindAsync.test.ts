import { expect } from 'chai'
import { Some, None } from '../types'
import { bindAsync } from './bindAsync'

describe('The bindAsync helper', () => {
    it('should return None for any callback when given None', async () => {
        // act
        const result = await bindAsync(async v => Some(v), None)

        // assert
        expect(result).to.equal(None)
    })

    describe('When given Some', () => {
        it('should return None if the callback returns None', async () => {
            // act
            const result = await bindAsync(async _ => None, Some(3))

            // assert
            expect(result).to.equal(None)
        })

        it('should return Some if the callback returns Some', async () => {
            // act
            const result = await bindAsync(async x => Some(x + 1), Some(3))

            // assert
            expect(result).to.equal(Some(4))
        })
    })
})
