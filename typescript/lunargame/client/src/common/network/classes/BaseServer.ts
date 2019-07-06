import { Account } from '../types/Account'
import { BehaviorSubject } from 'rxjs'
import { Singleton } from '@eix/utils'
import { defaultAvatar } from '../constants'

@Singleton
export class BaseServer {
    public account = new BehaviorSubject<Account | null>(null)

    constructor() {
        // mock account for now
        // this.account.next({
        //     name: 'Mock',
        //     email: 'mock@somethng.io',
        //     avatar: defaultAvatar,
        //     description: 'Just a random mock account',
        //     uid: '1234',
        //     verified: true
        // })
    }
}
