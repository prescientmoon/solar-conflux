import { ObjectSchema } from 'joi'
import { Middleware } from 'koa'
import { HttpError } from '../../../modules/network/classes/HttpError'

export const validate = (
    schema: ObjectSchema,
    field: 'params' | 'body' | 'query'
): Middleware => async (context, next) => {
    const result = schema.validate(
        field === 'body' ? context.request.body : context[field]
    )

    if (result.error) throw new HttpError(400, result.error.message)

    return next()
}
