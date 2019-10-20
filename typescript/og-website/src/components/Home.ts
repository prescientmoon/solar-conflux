import { html } from '@popeindustries/lit-html-server'
import { withCss } from '../helpers/withCss'
import { buttons } from '../constants/navButtons'
import { UrlConfig } from '../types/UrlConfig'

export const HomeButton = (config: UrlConfig) => html`
    <a class="home-button" href=${config.url}>${config.name}</a>
`

export const Home = () => html`
    ${withCss('home')}
    <div id="home" class="full center">
        <div id="home-title">
            Hello! I'm Matei Adriel!
        </div>

        <div id="home-buttons">
            ${buttons.filter(button => button.name !== 'Home').map(HomeButton)}
        </div>
    </div>
`
