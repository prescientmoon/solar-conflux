import { html, render } from "lit-html"
import { AdrielServer } from "./routes";
import { subscribe } from "lit-rx"

import "./lit-route"
import { map } from "rxjs/operators";

const password = "123"
const email = "rafaeladriel11@gmail.com"
const name = "Adriel Matei"

const server = new AdrielServer()

let timer:number[] = []

const signup = () => {
    timer.push(performance.now())
    server.signup(email, name, password)
}

render(html`
<lit-router>
    <lit-route href="/">
        <button @click=${signup}>Signup</button>
        <div>
            Logged in: ${subscribe(server.authenticated)} <br>
            It took: ${subscribe(server.authenticated.pipe(map(val => 
                performance.now() - timer[timer.length - 1]
            )))} ms
        </div>
    </lit-route>
    <lit-route href="/test">
        <br>
        something else
    </lit-route>
    <div>
        this isnt affected
    </div>
    <button>
        <lit-a to="/test">
            Test
        </lit-a>
    </button>
    <button>
        <lit-a to="/">
            Root
        </lit-a>
    </button>
</lit-router>
`, document.body)