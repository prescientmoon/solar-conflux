import { iNode_env } from './src/modules/core/node_env'
import { Config } from 'knex'
import { resolve } from 'path'

// This is the name of the db file
const dbName = 'db.sqlite'

// Ive made those to prevent repetition
const dbFolder = resolve(__dirname, 'db')
const testFolder = resolve(__dirname, 'test')

// This is used in all configs
const commonConfig: Partial<Config> = {
    migrations: {
        directory: resolve(dbFolder, 'migrations'),
        tableName: 'migrations'
    },
    useNullAsDefault: true
}

// This is the confg we are going to esport
// Im making a separate variable instead of
// default exporting it because i want to
// also eport each prop by name
const config: Partial<Record<iNode_env, Config>> = {
    development: {
        client: 'sqlite3',
        connection: {
            filename: resolve(dbFolder, dbName)
        },
        ...commonConfig,
        seeds: {
            directory: resolve(dbFolder, 'seeds')
        }
    },
    test: {
        client: 'sqlite3',
        connection: {
            filename: resolve(testFolder, dbName)
        },
        ...commonConfig,
        seeds: {
            directory: resolve(testFolder, 'seeds')
        }
    }
}

// These are exposed to knex
const { development, test } = config

// This is the export wich should be used in th eactua app
export default config

// For migartions to work
// If i dont include this knex will throw an error
export { development, test }
