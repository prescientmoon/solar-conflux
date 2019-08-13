import { requireAuthenticated } from './requireAuthenticated'
import { Context } from 'koa'
import { fakeNext } from '../../../../test/utils/fakeNext'

describe('The requireAuthenticated middleware', () => {
    test("should throw an error if the user isn't logged in", () => {
        // arrange
        const fakeContext = {
            session: {}
        } as Context

        // arrange
        const runMiddleware = () => requireAuthenticated()(fakeContext, fakeNext())

        // assert
        expect(runMiddleware).toThrow()
    })

    test('should call next if the user is logged in', () => {
        // arrange
        const fakeContext = ({
            session: {
                uid: Math.random()
            }
        } as unknown) as Context

        const next = jest.fn(fakeNext())

        // act
        requireAuthenticated()(fakeContext, next)

        // assert
        expect(next).toBeCalled()
    })
})
