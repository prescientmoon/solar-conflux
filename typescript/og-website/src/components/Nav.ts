import { TemplateResult, html } from '@popeindustries/lit-html-server'
import { withCss } from '../helpers/withCss'
import { UrlConfig } from '../types/UrlConfig'

export const NavButtons = (config: UrlConfig) => html`
    <a class="nav-button" href=${config.url}>${config.name}</a>
`

export const Nav = (buttons: UrlConfig[]) =>
    html`
        ${withCss('nav')}

        <div id="nav">
            <div id="nav-buttons">
                ${buttons.map(NavButtons)}
            </div>
        </div>
    `
