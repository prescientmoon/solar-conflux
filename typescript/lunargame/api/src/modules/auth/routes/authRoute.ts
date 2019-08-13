import Router from 'koa-router'
import { validate } from '../../../common/validation/middleware/validate'
import { getPasswordByEmail } from '../queries/getPasswordByEmail'
import { HttpError } from '../../network/classes/HttpError'
import { checkPassword } from '../helpers/checkPassword'
import { SignupBodySchema } from '../schemas/SignupBody'
import { encryptPassword } from '../helpers/encryptPassword'
import { createAccount } from '../queries/createAccount'
import { defaultEncryptionMethod } from '../constants'
import { LoginBodySchema } from '../schemas/LoginBody'
import { requireAnonymous } from '../middleware/requireAnonymous'

const router = new Router()

router.get('/', (context, next) => {
    context.body = {
        uid: context.session.uid
    }

    return next()
})

router.post(
    '/login',
    requireAnonymous(),
    validate(LoginBodySchema, 'body'),
    async (context, next) => {
        const { email, password } = context.request.body
        const passwordData = await getPasswordByEmail(email)

        // in case the user doesnt exist
        if (!passwordData) {
            throw new HttpError(404)
        }

        const match = await checkPassword(
            passwordData.password,
            password,
            passwordData.passwordEncryption
        )

        if (!match) {
            throw new HttpError(422, 'wrong password')
        }

        context.session.uid = passwordData.id
        context.body = {
            encryption: passwordData.passwordEncryption,
            uid: passwordData.id
        }

        return next()
    }
)

router.post(
    '/signup',
    requireAnonymous(),
    validate(SignupBodySchema, 'body'),
    async (context, next) => {
        const { email, name, password } = context.request.body

        // encript the password (bcrypt by default)
        const encryptedPassword = await encryptPassword(password, defaultEncryptionMethod, 10)

        const uid = await createAccount({
            email,
            name,
            password: encryptedPassword,
            passwordEncryption: defaultEncryptionMethod
        })

        context.body = {
            uid,
            encryption: defaultEncryptionMethod
        }

        return next()
    }
)

export default router
