import { Context } from 'koa'
import { isUnauthorized } from './isUnauthorized'

describe('The isUnauthorized middleware', () => {
    const fakeNext = () => async () => {}

    test('should throw an error if the user is logged in', () => {
        const fakeContext = ({
            session: {
                uid: 7
            }
        } as unknown) as Context

        expect(() => isUnauthorized()(fakeContext, fakeNext())).toThrow()
    })

    test("should call next if the user isn't logged in", () => {
        const fakeContext = {
            session: {}
        } as Context

        const next = jest.fn(fakeNext())

        isUnauthorized()(fakeContext, next)

        expect(next).toBeCalled()
    })
})
