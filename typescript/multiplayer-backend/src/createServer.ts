import * as express from "express"
import * as sessions from "express-session"
import chalk from "chalk";

import { staticRoutes } from "../config";
import { routes } from "./routes"
import { urlencoded } from "body-parser";
import { database } from "./services/db/firestore"
import { morganChalk } from "./middleware/morgan";
import { sessionMiddleware } from "./middleware/sessions"


// @ts-ignore no declaration file
// import * as store from "firestore-store"
import * as store from "connect-mongo"
import { connection, connected } from "./services/db/mongo";

// const firestore = store(sessions)
export interface serverSetupResults {
    app: express.Application
}

export const setupServer = (): Promise<serverSetupResults> =>
    new Promise(async (res, rej) => {
        try {
            let MongoStore = store(sessions)

            await connected

            //create express app
            const app = express()

            app.use(urlencoded({ extended: true }), sessions({
                secret: process.env.SESSION_SECRET,
                saveUninitialized: false,
                resave: false,
                store: new MongoStore({ mongooseConnection: connection })
            }), morganChalk, sessionMiddleware)

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