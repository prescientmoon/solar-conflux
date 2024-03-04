import { html } from '@popeindustries/lit-html-server'
import { withCss } from '../helpers/withCss'
import { Nav } from './Nav'
import { buttons } from '../constants/navButtons'
import { LayoutOptions } from '../types/LayoutOptions'
import { ServiceWorker } from './ServiceWorker'

export const Layout = ({ title, body, url }: LayoutOptions) => html`
    <!DOCTYPE html>
    <html lang="en">
        <head>
            <meta charset="UTF-8" />
            <title>${title}</title>
            <base href="/" />
            ${withCss('layout', 'config')}
        </head>
        <body class="background">
            ${Nav(buttons, url)}
            <div id="page-content">${body}</div>

            ${ServiceWorker('/static/js/service-worker.js')}
        </body>
    </html>
`
