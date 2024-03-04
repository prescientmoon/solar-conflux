import { component, html, useState } from "haunted"
import { router } from "./lit-router"


const Route = component((props) => {
    const [$html] = useState([...props.childNodes])

    return html`
        ${$html}
    `
})

const routerTagName = "lit-route"
customElements.define(routerTagName, Route)

const Router = component((props) => {
    let [currentPath, setCurrentPath] = useState("/")
    let [first, setFirst] = useState(true)

    const [children] = useState([...props.children])

    if (first) {
        router.use("*", (req, res, next) => {
            setCurrentPath(req.originalUrl)
            res.end()
        })

        router.listen()
        setFirst(false)
    }


    const safeChildren = children
        .filter((value: any) => value.tagName != routerTagName.toLocaleUpperCase())

    const routeElements: HTMLElement[] = children
        .filter((value: any) => value.getAttribute("href") == currentPath)

    return html`
        ${safeChildren}
        ${routeElements}
    `
})

customElements.define("lit-router", Router)

const litA = component((props:any) => {
    const [$html] = useState([...props.childNodes])
    const [url] = useState(props.getAttribute("to"))

    return html`
    <span @click=${() => { router.push(url) }}>
        ${$html}
    </span>`
})

customElements.define("lit-a", litA)