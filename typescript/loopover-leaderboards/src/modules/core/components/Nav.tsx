import { Block, Row } from "jsxstyle";
import { h } from "preact";
import { Link } from "preact-router/match";
import styles from "./Nav.css";

interface Route {
    url: string;
    name: string;
}

const routes: Route[] = [
    {
        url: "/",
        name: "Home"
    },
    {
        url: "/leaderboards",
        name: "Leaderboards"
    },
    {
        url: "/play",
        name: "Play"
    }
];

export const Nav = () => {
    return (
        <Row background="rgba(0,0,0, 0.3)">
            {routes.map(route => (
                // We cannot style this directly, so I'm using classes
                <Link
                    key={route.url}
                    href={route.url}
                    class={styles.link}
                    activeClassName={styles.activeLink}
                >
                    {route.name}
                </Link>
            ))}
        </Row>
    );
};
