export interface AdrielRequestOptions {
    method: string
    url: string
    body: any
}

export interface AdrielResponse {
    succes:boolean
    data: any
    errors?: string[]
}

export interface AdrielTokenResponse extends AdrielResponse {
    data: {
        token: string
    }
}

export interface User {
    uid:string,
    name: string,
    email: string,
    photoUrl: string,
    friends: string[]
}

