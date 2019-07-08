import { Account } from '../types/Account'
import { BehaviorSubject } from 'rxjs'
import { Singleton } from '@eix/utils'
import { Response } from '../types/Response'

@Singleton
export class BaseServer {
    public account = new BehaviorSubject<Account | null>(null)
    public path = 'http://localhost:8000'

    constructor() {
        this.refreshAccount()
    }

    public async refreshAccount(url = 'account', method = 'GET', body = {}) {
        try {
            const account = await this.request<Account>(url, method, body)
            this.account.next(account)
            return account
        } catch (err) {
            this.account.next(null)
            return null
        }
    }

    public async request<T>(
        url: string,
        method = 'GET',
        body = {}
    ): Promise<T> {
        const noBody = ['GET', 'DELETE']

        const response = await fetch(`${this.path}/${url}`, {
            ...(noBody.indexOf(method) === -1
                ? { body: JSON.stringify(body) }
                : {}),
            headers: {
                Accept: 'application/json',
                'Content-Type': 'application/json',
                'Access-Control-Allow-Credentials': 'true'
            },
            method,
            credentials: 'include'
        })
        const parsed: Response<T> = await response.json()
        const status = response.status

        if (status !== 200) {
            console.warn(parsed.message)
            throw new Error(parsed.message)
        }

        return parsed.data
    }
}
