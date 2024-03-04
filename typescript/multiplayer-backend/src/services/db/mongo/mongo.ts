import { config } from "dotenv"
import { connect, model, connection } from "mongoose"

config()
connect(process.env.DATABASE, { useNewUrlParser: true })

export const connected = new Promise((res, rej) => {
    connection.on("open", (...args: any[]) => {
        res(...args)
    })
    connection.on("error", (...args: any[]) => {
        rej(...args)
    })
})

export { connection }
