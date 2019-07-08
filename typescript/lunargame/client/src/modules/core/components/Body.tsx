import React from 'react'
import { SiddebarRoutes } from './SidebarRouteList'
import { makeStyles } from '@material-ui/styles'
import { Route } from 'react-router-dom'

const useStyles = makeStyles({
    root: {
        padding: '5%'
    }
})

export const Body = (props: unknown) => {
    const classes = useStyles(props)

    return (
        <div className={classes.root}>
            <SiddebarRoutes />
        </div>
    )
}
