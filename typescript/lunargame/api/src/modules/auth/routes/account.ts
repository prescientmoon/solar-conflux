import { getUserByUid } from '../queries/getUserById'
import Router from 'koa-router'
import { HttpError } from '../../network/classes/HttpError'
import { notGuest } from '../middleware/notGuest'
import { verifyAccount } from '../queries/verifyEmail'
import { mode } from '../../core/constants'
import { filterPrivateAccountData } from '../helpers/accountDataFilters'
import { validate } from '../../../common/validation/middleware/validate'
import { hasVerificationToken } from '../authSchemas'

const router = new Router()

router.get('/uid', notGuest(), async (context, next) => {
    const uid: string = context.session.uid

    context.body = {
        uid
    }

    next()
})

router.delete('/uid', (context, next) => {
    context.session.uid = undefined

    context.body = {
        succes: true
    }

    next()
})

router.get('/', notGuest(), async (context, next) => {
    const uid: string = context.session.uid
    const account = await getUserByUid(uid)

    if (!account) throw new HttpError(404)

    context.body = {
        data: filterPrivateAccountData(account)
    }

    next()
})

router.get(
    '/verify/:token',
    validate(hasVerificationToken, 'params'),
    async (context, next) => {
        const token = context.params.token
        await verifyAccount(token)

        context.body = `Succesfully verified account!`

        return next()
    }
)

export { router }
