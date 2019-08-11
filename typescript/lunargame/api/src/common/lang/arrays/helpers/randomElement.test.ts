import { randomElement } from './randomElement'

describe('The randomElement function', () => {
    test('should return the only element in an array of length 1', () => {
        const element = 7

        expect(randomElement([element])).toBe(element)
    })

    test('should throw an error when passing an empty array', () => {
        let error: Error | undefined

        try {
            randomElement([])
        } catch (catchedError) {
            //
            error = catchedError
        }

        expect(error).toBeTruthy()
    })
})
