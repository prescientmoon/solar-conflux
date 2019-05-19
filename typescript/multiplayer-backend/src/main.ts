import { config } from "dotenv"
import { setupServer } from "./createServer";
import { connected } from "./services/db/mongo"
import chalk from "chalk";

//⚓ connect to mongodb
connected.then(val => {
    console.log(chalk.bold.green("⚓  Succesfully connected to mongoDb!!!"))
}).catch(err => {
    console.log(err)
    console.log(chalk.bold.red("😭  Something went wrong when connecting to mongoDb!!!"))
    process.exit(1)
})

//🗻 extract env variables
config()

const main = async () => {

    //create the server
    const { app } = await setupServer()

    //start listeing to requests
    app.listen(process.env.PORT)
}

main()