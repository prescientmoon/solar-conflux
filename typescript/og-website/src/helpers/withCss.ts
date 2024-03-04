import { html } from '@popeindustries/lit-html-server'

export const withCss = (...names: string[]) =>
    names.map(
        name => html`
            <link
                type="text/css"
                rel="stylesheet"
                href=${`static/css/${name}.css`}
            />
        `
    )
