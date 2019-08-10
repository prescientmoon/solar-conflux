exports.up = function up(knex) {
    return knex.schema.createTable('game', table => {
        table.increments()
        table
            .text('name')
            .notNull()
            .defaultTo('myGame')
        table
            .text('avatar')
            .notNull()
            .defaultTo('')
        table
            .text('thumbail')
            .notNull()
            .defaultTo('')
        table
            .text('description')
            .notNull()
            .defaultTo('This is my game!')
        table
            .boolean('public')
            .notNull()
            .defaultTo(false)
    })
}

exports.down = function down(knex) {
    return knex.schema.dropTable('game')
}
