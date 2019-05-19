import * as express from "express"
import chalk from "chalk";

import { staticRoutes } from "../config";
import { routes } from "./routes"
import { urlencoded } from "body-parser";
import { morganChalk } from "./middleware/morgan";
import { sessionMiddleware } from "./middleware/sessions"

// const firestore = store(sessions)
export interface serverSetupResults {
    app: express.Application
}

export const setupServer = (): Promise<serverSetupResults> =>
    new Promise(async (res, rej) => {
        try {
            //create express app
            const app = express()

            app.use(urlencoded({ extended: true }), morganChalk, sessionMiddleware)

            //load static routes
            staticRoutes.forEach(route => {
                app.use(express.static(`${route}`))
            })

            //Load normal routes
            for (let i in routes) {
                app.use(`/${i}`, routes[i])
            }
            console.log(chalk.bold.green("👏  Succesfully creatd server!"))

            res({ app })
        }
        catch (err) {
            rej(err)
        }
    })