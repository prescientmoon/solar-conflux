import { Context } from 'koa'
import { requireAnonymous } from './requireAnonymous'
import { fakeNext } from '../../../../test/utils/fakeNext'

describe('The requireAnonymous middleware', () => {
    test('should throw an error if the user is logged in', () => {
        // act
        const fakeContext = ({
            session: {
                uid: 7
            }
        } as unknown) as Context

        // arrange
        const runMiddleware = () => requireAnonymous()(fakeContext, fakeNext())

        // assert
        expect(runMiddleware).toThrow()
    })

    test("should call next if the user isn't logged in", () => {
        // arrange
        const fakeContext = {
            session: {}
        } as Context

        const next = jest.fn(fakeNext())

        // act
        requireAnonymous()(fakeContext, next)

        // assert
        expect(next).toBeCalled()
    })
})
