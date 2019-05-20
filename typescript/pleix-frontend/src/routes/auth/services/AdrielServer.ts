import { AdrielRequestOptions, AdrielTokenResponse } from "./interfaces";
import { Subject } from "rxjs"
import { map } from "rxjs/operators"

export class AdrielServer {
    private _ready = new Subject()
    private token: string = "";

    private static defaultOptions: AdrielRequestOptions = {
        method: "GET",
        url: "/",
        body: {}
    }

    public url = "http://localhost:8000"
    public ready = this._ready.toPromise()

    public user = new Subject()
    public uid = new Subject<string | null>()
    public authenticated = this.uid.pipe(
        map(val => !!val)
    )

    constructor() { 
        const tokenPromise = new Promise<string>(async (res,rej) => {
            if (!localStorage.token) {
                const token = await this.generateToken()
                localStorage.token = token
                res(token) 
            }
            else 
                res(localStorage.token)
        })

        tokenPromise.then((token:string) => {
            this.token = token
            this._ready.next()
            this._ready.complete()
            this.uid.next(null)
        }).catch(err => {
            this._ready.error(err)
        })
    }

    private async generateToken() {
        const result = await fetch(`${this.url}/token`)
        const data: AdrielTokenResponse = await result.json()
        const token = data.data.token

        return token
    }

    async request(options: Partial<AdrielRequestOptions>) {
        await this.ready

        const finalOptions = { ...AdrielServer.defaultOptions, ...options }

        //stringify body
        finalOptions.body = JSON.stringify(finalOptions.body)

        const forbiddenKyes = ["url"]
        const optionMixin: Partial<AdrielRequestOptions> = {}

        for (let i in finalOptions) {
            //only for my editor to shut up
            const key = i as keyof AdrielRequestOptions

            if (forbiddenKyes.indexOf(key) == -1)
                optionMixin[key] = finalOptions[key]
        }

        console.log(optionMixin)

        //check if theres a token
        if (!this.token){
            throw new Error("cannot make a request without a token")
        }

        const result = await fetch(`${this.url}/${finalOptions.url}`, {
            ...optionMixin,
            headers: {
                'Content-Type': 'application/json',
                authorization: `Bearer ${this.token}`
            }
        })

        return result
    }

    async signup(email: string, name: string, password: string) {
        const res = await this.request({
            url: "auth",
            method: "POST",
            body: {
                password, email, name
            }
        })
        
        const data = await res.json()
    
        this.uid.next(data.data.uid)

        return data.data.uid
    }
}