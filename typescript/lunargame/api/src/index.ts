import { app } from './server'

const port = process.env.PORT

export const server = app.listen(Number(port), () => {
    console.log(`Listening on port ${port}`)
})
