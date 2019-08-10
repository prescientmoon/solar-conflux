import { router as accountRouter } from '../auth/routes/account'
import { router as authRouter } from '../auth/routes/auth'
import { router as userRouter } from '../auth/routes/user'
import { router as gameRouter } from '../game/routes/game'
import Router from 'koa-router'

const router = new Router()

router.use('/account', accountRouter.middleware())
router.use('/auth', authRouter.middleware())
router.use('/user', userRouter.middleware())
router.use('/game', gameRouter.middleware())

export { router }
