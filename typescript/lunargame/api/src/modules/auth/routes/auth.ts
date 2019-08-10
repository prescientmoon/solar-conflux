import uuid from 'uuid/v4'
import Router from 'koa-router'
import { getUserWithPassword } from '../queries/getAccountWithPassword'
import { checkPassword } from '../helpers/checkPassword'
import { HttpError } from '../../network/classes/HttpError'
import { isGuest } from '../middleware/isGuest'
import { createPassword } from '../queries/createPassword'
import { createUser } from '../queries/createUser'
import { encryptPassword } from '../helpers/encryptPassword'
import { uniqueEmail } from '../middleware/uniqueEmail'
import { EmailManager } from '../../network/classes/EmailManager'
import { subject, text } from '../helpers/verificationEmail'
import { validate } from '../../../common/validation/middleware/validate'
import { createUserSchema, hasEmail, loginSchema } from '../authSchemas'
import { filterPrivateAccountData } from '../helpers/accountDataFilters'

const router = new Router()
const emailManager = new EmailManager()

router.post(
    '/login',
    isGuest(),
    validate(loginSchema, 'body'),
    async (context, next) => {
        const account = await getUserWithPassword(
            'email',
            context.request.body.email
        )

        const password: string = context.request.body.password

        if (!account) {
            throw new HttpError(400, "Account does't exist")
        } else if (!(await checkPassword(account, password))) {
            throw new HttpError(400, 'Wrong password')
        }

        context.session.uid = account.uid
        context.body = {
            data: filterPrivateAccountData(account)
        }

        return next()
    }
)

router.post(
    '/create',
    isGuest(),
    validate(createUserSchema, 'body'),
    uniqueEmail(),
    async (context, next) => {
        const { email, name, password } = context.request.body

        const hash = await encryptPassword(10, password)
        const uid = uuid()
        const token = uuid()

        const [, account] = await Promise.all([
            createPassword(hash, uid),
            createUser(name, email, uid, token),
            emailManager.send(email, subject, text(token, name))
        ])

        context.session.uid = uid

        context.body = {
            data: filterPrivateAccountData(account[0])
        }

        return next()
    }
)

export { router }
