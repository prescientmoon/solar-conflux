import React from 'react'
import { SiddebarRoutes } from './SidebarRouteList'
import { makeStyles } from '@material-ui/styles'
import { Route } from 'react-router-dom'

const useStyles = makeStyles({
    root: {
        height: '90vh',
        display: 'block'
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
