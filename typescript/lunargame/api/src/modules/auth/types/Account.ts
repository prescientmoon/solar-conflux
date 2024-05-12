import { passwordEncryption } from './passwordEncryption'

/**
 * The data about an account wich needs to be inserted into the db
 */
export interface DbAccount {
    name: string
    email: string
    password: string
    passwordEncryption: passwordEncryption
}

/**
 * The data about an account wich actually gets stored into the db
 */
export interface FullDbAccount extends DbAccount {
    id: number
}

/**
 * The data everyone can get about an account
 */
export interface AccountPublicData {
    name: string
}

/**
 * The data only the owner of the account has acces to
 */
export interface AccountPrivateData extends AccountPublicData {
    email: string
}
