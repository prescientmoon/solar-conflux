import Koa, { Middleware } from 'koa'
import session from 'koa-session'
import knexSessionStore from 'koa-session-knex-store'
import { DbManager } from '../../db/classes/DbManager'

const { connection } = new DbManager()
const store = knexSessionStore(connection, {
    createtable: true
})

export const handleSessions = (app: Koa): Middleware =>
    session(
        {
            key: 'something',
            maxAge: 1000 * 60 * 60 * (24 * 7),
            overwrite: true,
            signed: true,
            rolling: true,
            renew: false,
            store,
            domain: 'localhost'
        },
        app
    )
