import config from '../../../knexfile'
import knex, { Config } from 'knex'
import { node_env } from '../core/node_env'

// TODO: remove the as Config after finshnig the knexfile
export const connection = knex(config[node_env] as Config)
