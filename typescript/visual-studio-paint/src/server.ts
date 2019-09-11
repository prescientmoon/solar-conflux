import express from "express"
import compression from "compression"
import * as sapper from "@sapper/server"
import "regenerator-runtime/runtime"
// import session from "express-session"
// import knex from "knex"
// import knexfile from "../knexfile"

// // @ts-ignore no types
// import createKnexSessionStore from "connect-session-knex"
// import bodyParser from "body-parser"

const { PORT, NODE_ENV, SECRET } = process.env

const dev = NODE_ENV === "development"

// const KnexSessionStore = createKnexSessionStore(session)
// const connection = knex(knexfile[NODE_ENV || "development"])
// const store = new KnexSessionStore({
//   knex: connection
// })

const app = express()

app.use(
  // session({
  //   secret: SECRET || "secret",
  //   cookie: {
  //     maxAge: 1000 * 60 * 60 * 24 * 7
  //   },
  //   store
  // }),
  // bodyParser.json(),
  compression({ threshold: 0 }),
  express.static("static"),
  sapper.middleware()
)

app.listen(PORT, () => {
  console.log(`Listening on port ${PORT}`)
})
