import express from 'express'
import { Layout } from './components/Layout'
import { resolve } from 'path'
import { createPageMiddlewareFactory } from './middleware/servePage'
import { Home } from './components/Home'
import { Projects } from './components/Projects'

const port = process.env.PORT || 8080
const app = express()

const renderComponent = createPageMiddlewareFactory(Layout, 'Matei Adriel')

app.use('/static', express.static(resolve(__dirname, 'static')))

app.get(
    '/',
    renderComponent({
        body: Home(),
        title: 'Home'
    })
)

app.get(
    '/projects',
    renderComponent({
        body: Projects(),
        title: 'Projects'
    })
)

app.listen(port, () => {
    console.log(`Listening on port ${port}`)
})
