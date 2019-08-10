import Router from 'koa-router'
import { hasName } from '../authSchemas'
import { validate } from '../../../common/validation/middleware/validate'
import { getPublicAccountData } from '../queries/getPublicAccountData'
import { HttpError } from '../../network/classes/HttpError'

const router = new Router()

router.get(
    '/name/:name',
    validate(hasName, 'params'),
    async (context, next) => {
        const name: string = context.params.name
        const account = await getPublicAccountData('name', name)

        if (!account) throw new HttpError(404, `User ${name} does not exist.`)

        context.body = {
            data: account
        }

        return next()
    }
)

export { router }
