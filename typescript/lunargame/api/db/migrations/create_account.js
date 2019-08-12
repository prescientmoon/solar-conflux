// in case i want to change it
// it's alwys a pain to change it everywhere
const tableName = 'account'

exports.up = knex => {
    return knex.schema.createTable(tableName, table => {
        // this is the id of the simulation
        table.increments()

        // the name of the user
        table.text('name').notNullable()

        // the email of the user
        table.text('email').notNullable()

        // the password of the user
        table.text('password').notNullable()

        // the password encryption
        table.text('passwordEncryption').notNullable()
    })
}

exports.down = knex => {
    return knex.schema.dropTable(tableName)
}
