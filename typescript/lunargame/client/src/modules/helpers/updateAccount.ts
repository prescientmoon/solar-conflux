import { BaseServer } from '../network/classes/BaseServer'
import { Account } from '../network/types/Account'

const server = new BaseServer()

export const updateAccount = (data: Account | null) => {
    server.account.next(data)
}
