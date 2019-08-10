import { Middleware } from 'koa'
import { HttpError } from '../../network/classes/HttpError'
import { mode } from '../../core/constants'

const strongRegex = new RegExp(
    '^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[!@#$%^&*])(?=.{8,})'
)

export const validatePassword = (prouctondOnly = true): Middleware => (
    context,
    next
) => {
    const password = context.request.body.password

    if (!password) {
        throw new HttpError(400, 'No password recived.')
    } else if (
        (mode === 'production' || !prouctondOnly) &&
        !strongRegex.test(context.request.body.password)
    ) {
        throw new HttpError(400, 'Bad password.')
    } else {
        return next()
    }
}
