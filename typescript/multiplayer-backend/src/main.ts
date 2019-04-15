import { config } from "dotenv"
import { setupServer } from "./createServer";

//extract env variables
config()

//create the server
const {app} = setupServer()

//start listeing to requests
app.listen(process.env.PORT)
