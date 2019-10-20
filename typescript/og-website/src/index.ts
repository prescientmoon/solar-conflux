import express from 'express'
import { Layout } from './components/Layout'
import { resolve } from 'path'
import { createPageMiddlewareFactory } from './middleware/servePage'
import { buttons } from './constants/navButtons'

const port = process.env.PORT || 8080
const app = express()

const renderComponent = createPageMiddlewareFactory(Layout, 'Matei Adriel')

app.use('/static', express.static(resolve(__dirname, 'static')))

for (const button of buttons) {
    app.get(
        button.url,
        renderComponent({
            body: button.component(),
            title: button.name,
            url: button.url
        })
    )
}

app.listen(port, () => {
    console.log(`Listening on port ${port}`)
})
