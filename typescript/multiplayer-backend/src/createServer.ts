import * as express from "express"
import { baseUrl, routes } from "../config";
import { performance } from "perf_hooks"
import { shortLogger } from "./routes/logging/shortLog";

export interface serverSetupResults{
    time:number;
    app:express.Application
}

export const setupServer = ():serverSetupResults => {

    const start = performance.now()
    const app = express()

    for (let i in routes) {
        const route = require(`${process.cwd()}/${baseUrl}${routes[i]}`)
        app.use(i, route.router)
    }

    const time = performance.now() - start
    const message = `Server created in: ${Math.floor(time)}ms`
    shortLogger.log("Server created",message)

    return {app,time}
}