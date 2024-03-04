import { TemplateResult, html } from '@popeindustries/lit-html-server'
import { withCss } from '../helpers/withCss'
import { UrlConfig } from '../types/UrlConfig'

export const NavButtons = (config: UrlConfig, url: string) => {
    const className = ['nav-button', config.url === url && 'glow']
        .filter(Boolean)
        .join(' ')

    return html`
        <a class=${className} href=${config.url}>${config.name}</a>
    `
}

export const Nav = (buttons: UrlConfig[], url: string) =>
    html`
        ${withCss('nav')}

        <div id="nav">
            <div id="nav-buttons">
                ${buttons.map(button => NavButtons(button, url))}
            </div>
        </div>
    `
