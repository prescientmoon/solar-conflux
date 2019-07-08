import React from 'react'
import { useObservable } from 'rxjs-hooks'
import { BaseServer } from '../../network/classes/BaseServer'
import Avatar from '@material-ui/core/Avatar'
import { makeStyles, Theme } from '@material-ui/core/styles'
import { LoginModal } from './LoginModal'
import { ModalButton } from './ModalButton'
import { SignupModal } from './SignupModal'

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
            <ModalButton modal={SignupModal}>Sign up</ModalButton>
            <ModalButton
                modal={LoginModal}
                className={classes.loginButton}
                contained
            >
                Log in
            </ModalButton>
        </>
    )

    return accountSnapshot ? (
        <Avatar src={accountSnapshot.avatar} alt={accountSnapshot.name} />
    ) : (
        signup
    )
}
