import Koa, { Middleware } from 'koa'
import session from 'koa-session'
import knexSessionStore from 'koa-session-knex-store'
import { connection } from '../../db/connection'

// The store sessions are saved to
const store = knexSessionStore(connection, {
    createtable: true
})

/**
 * Middleware factory for handling sessions
 *
 * @param app The app to handle sessions for
 */
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
