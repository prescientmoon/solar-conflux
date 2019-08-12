import * as Knex from 'knex'
import { DbAccount } from '../../src/modules/auth/types/Account'

const tableName = 'account'

export const mockAccounts: DbAccount[] = [
    {
        name: 'Adriel',
        email: 'rafaeladriel11@gmail.com',
        password: '1234',
        passwordEncryption: 'plain'
    }
]

export async function seed(knex: Knex): Promise<any> {
    return knex(tableName)
        .del()
        .then(() => {
            return knex(tableName).insert(mockAccounts)
        })
}
