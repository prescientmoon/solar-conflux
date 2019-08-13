import { Middleware } from 'koa'
import { HttpError } from '../../network/classes/HttpError'

/**
 * Middlware wich throws an error if the user isn't logged in
 */
export const requireAuthenticated = (): Middleware => (context, next) => {
    if (context.session.uid !== undefined) {
        return next()
    } else {
        throw new HttpError(401)
    }
}
