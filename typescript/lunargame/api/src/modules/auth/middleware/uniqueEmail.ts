import { Middleware } from 'koa'
import { emailIsTaken } from '../queries/emailIsTaken'
import { HttpError } from '../../network/classes/HttpError'

export const uniqueEmail = (): Middleware => async (context, next) => {
    const { email } = context.request.body

    if (!(await emailIsTaken(email))) {
        return next()
    }

    throw new HttpError(400, 'Email is already in use.')
}
