import { ObjectSchema } from 'joi'
import { Middleware } from 'koa'
import { HttpError } from '../../../modules/network/classes/HttpError'

/**
 * Middlware to validate a joi schema against a request
 *
 * @param schema The joi shcema to use for the validation
 * @param field The field to validate the schema against
 *
 * @throws HttpError if the validation fails
 */
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
