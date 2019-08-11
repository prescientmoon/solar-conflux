// in case i want to change it
// it's alwys a pain to change it everywhere
const tableName = 'simulation'

exports.up = function up(knex) {
    return knex.schema.createTable(tableName, table => {
        // this is the id of the simulation
        table.increments()

        // this is the actual name of the simulation
        table.text('name').notNull()
    })
}

exports.down = function down(knex) {
    return knex.schema.dropTable(tableName)
}
