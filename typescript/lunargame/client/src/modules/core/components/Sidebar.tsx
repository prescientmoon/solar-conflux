import React from 'react'
import List from '@material-ui/core/List'
import ListItem from '@material-ui/core/ListItem'
import ListItemIcon from '@material-ui/core/ListItemIcon'
import ListItemText from '@material-ui/core/ListItemText'
import { routes } from './SidebarRouteData'
import { Link } from 'react-router-dom'

export const Sidebar = () => {
    return (
        <>
            <List>
                {routes.map((route, index) => {
                    return (
                        <Link className="routeLink" to={route.url} key={index}>
                            <ListItem button>
                                <ListItemIcon>{route.icon}</ListItemIcon>
                                <ListItemText primary={route.name} />
                            </ListItem>
                        </Link>
                    )
                })}
            </List>
        </>
    )
}
