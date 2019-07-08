import React from 'react'
import Typography from '@material-ui/core/Typography'
import Divider from '@material-ui/core/Divider'
import { makeStyles, Theme } from '@material-ui/core/styles'
import { BaseServer } from '../../network/classes/BaseServer'

const useStyles = makeStyles((theme: Theme) => ({
    root: {
        flexGrow: 1
    },
    divider: {
        marginBottom: theme.spacing(2),
        marginTop: theme.spacing(2)
    },
    a: {
        color: '#0000ff'
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
                game engine. The project is open source, and right now also
                unusable.
            </Typography>

            <br />

            <Typography variant="h6" color="textSecondary">
                The project is open source on{' '}
                <a
                    className={classes.a}
                    href="https://github.com/Mateiadrielrafael/lunarbox-client"
                >
                    github
                </a>
                .
            </Typography>

            {/* Todo: remove */}
            <button
                onClick={async () => {
                    const server = new BaseServer()
                    await server.request('account/uid', 'DELETE')

                    server.account.next(null)
                }}
            >
                logout
            </button>
        </>
    )
}
