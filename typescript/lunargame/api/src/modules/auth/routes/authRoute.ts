import Router from 'koa-router'

const router = new Router()

router.get('/', (context, next) => {
    context.body = {
        uid: context.session.uid
    }

    return next()
})

router.post('/login', (context, next) => {
    context.session.uid = context.request.body.uid
    context.body = {}

    return next()
})

export default router
