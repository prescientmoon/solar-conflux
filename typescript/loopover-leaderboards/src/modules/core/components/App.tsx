import { h } from "preact";
import { Route, Router } from "preact-router";
import { Home } from "./Home";
import { Nav } from "./Nav";
import { Layer, Stack } from "./Stack";

if ((module as any).hot) {
    // tslint:disable-next-line:no-var-requires
    require("preact/debug");
}

export const App: preact.FunctionalComponent = () => {
    return (
        <div id="app">
            <Stack height="100vh">
                <Layer>
                    <Router>
                        <Route path="/" component={Home} />
                    </Router>
                </Layer>

                <Layer>
                    <Nav />
                </Layer>
            </Stack>
        </div>
    );
};
