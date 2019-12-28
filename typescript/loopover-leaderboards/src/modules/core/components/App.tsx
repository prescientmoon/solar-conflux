import { h } from "preact";
import { Route, Router } from "preact-router";

import { Home } from "./Home";

if ((module as any).hot) {
    // tslint:disable-next-line:no-var-requires
    require("preact/debug");
}

export const App: preact.FunctionalComponent = () => {
    return (
        <div id="app">
            <Router>
                <Route path="/" component={Home} />
            </Router>
        </div>
    );
};
