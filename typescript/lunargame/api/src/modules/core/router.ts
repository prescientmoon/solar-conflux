import Router from 'koa-router'
import AuthRouter from '../auth/routes/authRoute'

const router = new Router()

router.use('/auth', AuthRouter.middleware())

export { router }
