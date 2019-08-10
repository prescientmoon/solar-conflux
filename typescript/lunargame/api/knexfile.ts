export const development = {
    client: 'pg',
    connection: {
        port: '5432',
        host: 'localhost',
        database: 'lunarbox',
        user: 'postgres',
        password: 'drielrafael11'
    },
    migrations: {
        directory: './migrations',
        tablename: 'migrations'
    },
    seeds: {
        directory: './seeds'
    }
}
