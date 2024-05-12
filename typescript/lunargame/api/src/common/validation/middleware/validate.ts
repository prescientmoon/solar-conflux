import { ObjectSchema } from 'joi'
import { Middleware } from 'koa'
import { HttpError } from '../../../modules/network/classes/HttpError'

/**
 * The field wich the validate validator can use
 */
export type validationField = 'params' | 'body' | 'query'

/**
 * Middlware to validate a joi schema against a request
 *
 * @param schema The joi shcema to use for the validation
 * @param field The field to validate the schema against
 *
 * @throws HttpError if the validation fails
 */
export const validate = (schema: ObjectSchema, field: validationField): Middleware => (
    context,
    next
) => {
    const result = schema.validate(field === 'body' ? context.request.body : context[field], {
        abortEarly: true
    })

    if (result.error !== null) {
        throw new HttpError(422, result.error.message)
    }

    return next()
}
