import { Config } from "knex"
import { resolve } from "path"

// This is the name of the db file
const dbName = "db.sqlite"

// Ive made those to prevent repetition
const dbFolder = resolve(__dirname, "db")

// This is used in all configs
const commonConfig: Partial<Config> = {
  migrations: {
    directory: resolve(dbFolder, "migrations"),
    tableName: "migrations"
  },
  useNullAsDefault: true
}

// This is the confg we are going to esport
// Im making a separate variable instead of
// default exporting it because i want to
// also eport each prop by name
const config: Record<string, Config> = {
  development: {
    client: "sqlite3",
    connection: {
      filename: resolve(dbFolder, dbName)
    },
    ...commonConfig,
    seeds: {
      directory: resolve(dbFolder, "seeds")
    }
  }
}

// These are exposed to knex
const { development } = config

// This is the export wich should be used in th eactua app
export default config

// For migartions to work
// If i dont include this knex will throw an error
export { development }
