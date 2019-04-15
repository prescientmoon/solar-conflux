import { performance } from "perf_hooks"
import { config } from "dotenv";
import { shortConsole } from "./interfaces";
config()

//clear the console
if (process.env.MODE == "DEV")
    console.clear()

//used to log things
const shortLogger: shortConsole = {

    //holds all the logs
    logs: [],

    //log a new message
    log: (title: string, body: string) => {
        //push the logs to the log array     
        shortLogger.logs.push({
            title,
            body,
            time: new Date(),
            elapsed: Math.floor(performance.now())
        })

        //if we are in dev mode log it
        if (process.env.MODE == "DEV")
            console.log(body)
    }
}

export { shortLogger }