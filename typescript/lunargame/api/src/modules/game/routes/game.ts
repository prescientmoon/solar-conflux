import Router from 'koa-router'
import { validate } from '../../../common/validation/middleware/validate'
import { getGameCount } from '../queries/getGameCount'
import { chunkSchema } from '../gameSchemas'
import { getGameChunk } from '../queries/getGameChunk'

const router = new Router()

router.get('/count', async (context, next) => {
    const result = await getGameCount()

    context.body = {
        data: result
    }

    return next()
})

router.get('/chunk', validate(chunkSchema, 'query'), async (context, next) => {
    const { page, pageSize } = context.request.query

    context.body = {
        data: await getGameChunk(page, pageSize)
    }

    return next()
})

export { router }
