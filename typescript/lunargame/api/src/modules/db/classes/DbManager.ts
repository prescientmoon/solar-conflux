import { Singleton } from '@eix/utils'
import * as config from '../../../../knexfile'
import knex from 'knex'

@Singleton
export class DbManager {
    public mode: 'development' = 'development'
    public connection = knex(config[this.mode])
}
