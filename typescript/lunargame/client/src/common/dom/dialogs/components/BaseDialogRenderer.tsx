import React from 'react'
import Dialog from '@material-ui/core/Dialog'
import DialogTitle from '@material-ui/core/DialogTitle'
import { DialogManager } from '../classes/DialogManager'
import { useObservable } from 'rxjs-hooks'
import DialogActions from '@material-ui/core/DialogActions'
import DialogContent from '@material-ui/core/DialogContent'
import DialogContentText from '@material-ui/core/DialogContentText'
import { Button } from '@material-ui/core'

const manager = new DialogManager()

export const BaseDialogRenderer = () => {
    const activeDialog = useObservable(() => manager.active)

    if (activeDialog !== null) {
        return (
            <Dialog
                onClose={activeDialog.onClose}
                open={true}
                aria-labelledby="alert-dialog-title"
                aria-describedby="alert-dialog-description"
            >
                <DialogTitle id="alert-dialog-title">
                    {activeDialog.title}
                </DialogTitle>

                <DialogContent>
                    <DialogContentText id="alert-dialog-description">
                        {activeDialog.message}
                    </DialogContentText>
                </DialogContent>

                <DialogActions>
                    {activeDialog.actions.map((action, index) => {
                        return (
                            <Button
                                onClick={event => {
                                    action.callback()
                                    activeDialog.onClose(event)
                                }}
                                key={index}
                            >
                                {action.name}
                            </Button>
                        )
                    })}
                </DialogActions>
            </Dialog>
        )
    }

    return <></>
}
