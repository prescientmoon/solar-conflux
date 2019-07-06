import React from 'react'
import { useObservable } from 'rxjs-hooks'
import { BaseServer } from '../../network/classes/BaseServer'
import Avatar from '@material-ui/core/Avatar'
import Button from '@material-ui/core/Button'
import { makeStyles, Theme } from '@material-ui/core/styles'
import { Link } from 'react-router-dom'

const { account } = new BaseServer()

const useStyles = makeStyles((theme: Theme) => ({
    loginButton: {
        marginLeft: theme.spacing(2)
    }
}))

export const TopbarAccount = (props: unknown) => {
    const accountSnapshot = useObservable(() => account, null)

    const classes = useStyles(props)

    const signup = (
        <>
            <Button>
                <Link to="/signup">Sign up</Link>
            </Button>

            <Button variant="contained" className={classes.loginButton}>
                <Link to="/login"> Login</Link>
            </Button>
        </>
    )

    return accountSnapshot ? (
        <Avatar src={accountSnapshot.avatar} alt={accountSnapshot.name} />
    ) : (
        signup
    )
}
