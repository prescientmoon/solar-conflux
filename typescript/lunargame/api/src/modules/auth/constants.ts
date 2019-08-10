import { Account } from './types/Account'

export type AccountField = keyof Account

export const publicAccountFields: AccountField[] = [
    'name',
    'email',
    'description',
    'avatar'
]
export const privateAccountFields: AccountField[] = [
    ...publicAccountFields,
    'verified',
    'uid'
]
