import { Middleware } from 'koa'
import { httpSymbol } from '../../network/classes/HttpError'

/**
 * Midlware for error handling
 *
 * Not testing it because its made by Enitoni
 */
export const handleError = (): Middleware => async (context, next) => {
    try {
        await next()
    } catch (error) {
        if (error[httpSymbol]) {
            context.status = error.status
            context.body = {
                message: error.reason
            }

            return
        }

        console.log(error)

        context.status = 500
        context.body = 'Internal server error'
    }
}
