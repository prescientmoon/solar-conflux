exports.up = function up(knex) {
    return knex.schema.createTable('user-password', table => {
        table.increments()
        table.text('uid').notNull()
        table.text('value').notNull()
        table
            .boolean('secure')
            .defaultTo(true)
            .notNull()
    })
}

exports.down = function down(knex) {
    return knex.schema.dropTable('user-password')
}
