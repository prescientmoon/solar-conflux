import express from "express"
import compression from "compression"
import * as sapper from "@sapper/server"
import "regenerator-runtime/runtime"

const PORT = Number(process.env.PORT) || 3000

const app = express()

app.use(
  compression({ threshold: 0 }),
  express.static("static"),
  sapper.middleware()
)

app.listen(PORT, () => {
  console.log(`Listening on port ${PORT}`)
})
