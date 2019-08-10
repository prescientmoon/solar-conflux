import { Middleware } from 'koa'
import { HttpError } from '../../network/classes/HttpError'

export const isGuest = (): Middleware => (context, next) => {
    if (context.session.uid === null || context.session.uid === undefined)
        return next()

    throw new HttpError(400, 'Logged in.')
}
