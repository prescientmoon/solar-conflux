import { HttpError, HTTP_REASONS, HttpStatus, httpSymbol } from './HttpError'

describe('The HttpError class', () => {
    test('should allow passing a custom message', () => {
        const status = Math.random()
        const reason = 'testing'

        const error = new HttpError(status, reason)

        expect(error.toString()).toBe(`HttpError: ${status} - ${reason}`)
    })

    test('should use the default reason for the status when passing no second arg', () => {
        // ts will always consider it a string
        for (let untypedStatus in HTTP_REASONS) {
            // this forces ts to belive its an actual status
            const status = (untypedStatus as unknown) as HttpStatus
            const error = new HttpError(status)

            expect(error.reason).toBe(HTTP_REASONS[status])
        }
    })

    test('should always have the http error symbol set to true', () => {
        const error = new HttpError()

        expect(error[httpSymbol]).toBe(true)
    })
})
