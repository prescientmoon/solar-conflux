import React, { useState, KeyboardEvent } from 'react'
import MuiAppBar from '@material-ui/core/AppBar'
import Toolbar from '@material-ui/core/Toolbar'
import Typography from '@material-ui/core/Typography'
import IconButton from '@material-ui/core/IconButton'
import MenuIcon from '@material-ui/icons/Menu'
import SwipeableDrawer from '@material-ui/core/SwipeableDrawer'
import { Sidebar } from './Sidebar'
import { makeStyles, Theme } from '@material-ui/core/styles'
import { TopbarAccount } from '../../account/components/TopbarAccount'

const useStyles = makeStyles((theme: Theme) => ({
    root: {
        flexGrow: 1
    },
    title: {
        flexGrow: 1
    },
    list: {
        width: 250
    }
}))

export const AppBar = (props: unknown) => {
    const [sidebar, setSidebar] = useState(false)
    const classes = useStyles(props)

    const closeSidebar = () => setSidebar(false)
    const openSidebar = () => setSidebar(true)

    const handleKeydown = (open: boolean) => (
        event: KeyboardEvent<HTMLDivElement>
    ) => {
        if (event.key === 'Tab' || event.key === 'Shift') {
            return
        }

        setSidebar(open)
    }

    return (
        <>
            <div className={classes.root}>
                <MuiAppBar position="static">
                    <Toolbar>
                        <IconButton
                            edge="start"
                            color="inherit"
                            aria-label="Menu"
                            onClick={() => setSidebar(true)}
                        >
                            <MenuIcon />
                        </IconButton>
                        <Typography variant="h6" className={classes.title} />

                        <TopbarAccount />
                    </Toolbar>
                </MuiAppBar>
            </div>
            <SwipeableDrawer
                open={sidebar}
                onClose={closeSidebar}
                onOpen={openSidebar}
            >
                <div
                    role="presentation"
                    onClick={closeSidebar}
                    onKeyDown={handleKeydown(false)}
                    className={classes.list}
                >
                    <Sidebar />
                </div>
            </SwipeableDrawer>
        </>
    )
}
