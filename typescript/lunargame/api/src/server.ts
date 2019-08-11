import Koa from 'koa'
import cors from '@koa/cors'
import parser from 'koa-bodyparser'

import { config } from 'dotenv'
import { handleError } from './modules/network/middleware/errorHandler'
import { handleSessions } from './modules/network/middleware/handleSessions'
import { router } from './modules/core/router'

config()

const app = new Koa()

app.keys = [process.env.secret || 'secret']

app.use(
    cors({
        credentials: true
    })
)
    .use(handleError())
    .use(parser())
    .use(handleSessions(app))
    .use(router.middleware())

export { app }
