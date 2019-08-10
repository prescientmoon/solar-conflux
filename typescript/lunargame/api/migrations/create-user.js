exports.up = function up(knex) {
    return knex.schema.createTable('account', table => {
        table.text('uid').primary()
        table.text('verificationToken').notNullable()
        table.text('name').notNullable()
        table.text('email').notNullable()
        table
            .text('description')
            .defaultTo('')
            .notNullable()
        table
            .text('avatar')
            .defaultTo('')
            .notNullable()
        table
            .boolean('verified')
            .defaultTo(false)
            .notNullable()
    })
}

exports.down = function down(knex) {
    return knex.schema.dropTable('account')
}
