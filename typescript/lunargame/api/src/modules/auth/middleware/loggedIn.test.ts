import { isAuthorized } from './isAuthorized'
import { Context } from 'koa'

describe('The isAuthorized middleware', () => {
    const fakeNext = () => async () => {}

    test("should throw an error if the user isn't logged in", () => {
        const fakeContext = {
            session: {}
        } as Context

        expect(() => isAuthorized()(fakeContext, fakeNext())).toThrow()
    })

    test('should call next if the user is logged in', () => {
        const fakeContext = ({
            session: {
                uid: Math.random()
            }
        } as unknown) as Context

        const next = jest.fn(fakeNext())

        isAuthorized()(fakeContext, next)

        expect(next).toBeCalled()
    })
})
