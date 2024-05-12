import { Middleware } from 'koa'
import { HttpError } from '../../network/classes/HttpError'

/**
 * Middleware wich throws an error if the user is logged in
 */
export const requireAnonymous = (): Middleware => (context, next) => {
    if (context.session.uid === undefined) {
        return next()
    } else {
        throw new HttpError(401)
    }
}
