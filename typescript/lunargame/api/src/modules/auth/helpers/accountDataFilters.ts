import { privateAccountFields } from '../constants'
import { Account } from '../types/Account'

export const filterPrivateAccountData = <T>(account: Account & T) => {
    const result: Record<string, unknown> = {}

    for (const key of Object.keys(account)) {
        // for ts to shut up
        const typedKey = (key as unknown) as keyof Account

        if (privateAccountFields.includes(typedKey)) {
            result[typedKey] = account[typedKey]
        }
    }

    return result
}
