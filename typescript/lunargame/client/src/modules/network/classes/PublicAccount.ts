import { cacheInstances } from '../../../common/lang/objects/decorators/cacheInstances'
import { BehaviorSubject } from 'rxjs'
import { AccountPublicData } from '../types/AccountPublicData'
import { BaseServer } from './BaseServer'

@cacheInstances()
export class PublicAccount {
    public static server = new BaseServer()
    public account = new BehaviorSubject<AccountPublicData | null>(null)

    public constructor(public name: string) {}

    public init() {
        this.refresh()
    }

    public async refresh() {
        try {
            const account = await PublicAccount.server.request<
                AccountPublicData
            >(`user/name/${this.name}`)

            this.account.next(account)
            return account
        } catch {
            this.account.next(null)
            return null
        }
    }
}
