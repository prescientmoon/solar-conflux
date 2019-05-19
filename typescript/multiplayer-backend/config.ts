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
export const whiteList: whiteListUrl[] = [{
    url: "localhost:4200"
},{
    url: "localhost:8000"
},{
    url: "localhost:3000"
}]

export const staticRoutes:string[] = []