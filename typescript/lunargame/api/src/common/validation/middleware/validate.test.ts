import Joi from 'joi'
import { validate, validationField } from './validate'
import { Context } from 'koa'
import { fakeNext } from '../../../../test/utils/fakeNext'

describe('The validate middleware', () => {
    const schema = Joi.object({
        name: Joi.required()
    })

    const fields: validationField[] = ['body', 'params', 'query']

    for (const field of fields) {
        describe(`The request ${field} validator`, () => {
            const middleware = validate(schema, field)

            const getContext = (name?: number) => {
                if (field === 'body') {
                    return {
                        request: {
                            body: {
                                name
                            }
                        }
                    } as Context
                } else {
                    return {
                        [field]: {
                            name
                        }
                    } as Context
                }
            }

            test('should throw an error if the validation fails', () => {
                // arrange
                const context = getContext()

                // act
                const check = () => {
                    middleware(context, fakeNext())
                }

                // assert
                expect(check).toThrow()
            })

            test('should call next if the validation passed', () => {
                // arrange
                const context = getContext(7)

                const next = jest.fn(fakeNext())

                // act
                middleware(context, next)

                // assert
                expect(next).toBeCalled()
            })
        })
    }
})
