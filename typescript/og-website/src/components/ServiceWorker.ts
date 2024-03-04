import { html } from '@popeindustries/lit-html-server'

export const ServiceWorker = (url: string) => html`
    <script>
        if ('serviceWorker' in navigator) {
            window.addEventListener('load', () => {
                navigator.serviceWorker.register('${url}')
            })
        }
    </script>
`
