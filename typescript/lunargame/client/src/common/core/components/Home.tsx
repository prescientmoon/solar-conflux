import React from 'react'
import Typography from '@material-ui/core/Typography'
import Divider from '@material-ui/core/Divider'
import { makeStyles, Theme } from '@material-ui/core/styles'

const useStyles = makeStyles((theme: Theme) => ({
    root: {
        flexGrow: 1
    },
    divider: {
        marginBottom: theme.spacing(2),
        marginTop: theme.spacing(2)
    }
}))

export const Home = (props: unknown) => {
    const classes = useStyles(props)

    return (
        <>
            <Typography variant="h4">This is Lunarbox</Typography>

            <Divider className={classes.divider} />

            <Typography variant="h6" color="textSecondary">
                Lunarbox is a game streaming website for games made with the eix
                game engine. The project is open source, and right now it's
                unusable.
            </Typography>
        </>
    )
}
