import { Middleware } from 'koa'
import { HttpError } from '../../network/classes/HttpError'

export const notGuest = (): Middleware => (context, next) => {
    if (context.session.uid === null || context.session.uid === undefined)
        throw new HttpError(401)

    return next()
}
