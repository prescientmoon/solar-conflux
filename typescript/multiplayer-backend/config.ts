//public config

//interfaces
export interface routeList {
    [key: string]: string
}
export interface whiteListUrl {
    url: string
    methods?: string
}

//routes
export const baseUrl = "./src/routes/"
export const routes: routeList = {
    "/logs": "logging/logs.ts",
    "/auth": "auth/auth.ts"
}

//whitelist of urls
export const whiteList = [{
    url: "http://localhost:4200"
}]