import React from 'react'
import { routes } from './SidebarRouteData'
import { Route } from 'react-router-dom'

export const SiddebarRoutes = () => {
    return (
        <>
            {routes.map((route, index) => (
                <Route
                    key={index}
                    path={route.url}
                    component={route.content}
                    exact={route.url === '/'}
                />
            ))}
        </>
    )
}
