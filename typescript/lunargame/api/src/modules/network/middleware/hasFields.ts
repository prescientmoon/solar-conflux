import { Middleware } from 'koa'
import { HttpError } from '../classes/HttpError'

export const hasFields = (
    ...fields: string[]
): Middleware<{ field: string }> => async (context, next) => {
    for (const value of fields) {
        if (context.request.body[value]) {
            context.state.field = value
            return next()
        }
    }

    throw new HttpError(400, `None of the fields ${fields.join(' ')} included.`)
}
