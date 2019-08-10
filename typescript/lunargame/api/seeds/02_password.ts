import * as Knex from 'knex'

export async function seed(knex: Knex): Promise<any> {
    return knex('user-password')
        .del()
        .then(() => {
            return knex('user-password').insert([
                { uid: 1, value: '7777', secure: false }
            ])
        })
}
