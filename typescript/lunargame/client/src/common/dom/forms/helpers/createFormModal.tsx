import React from 'react'
import Dialog from '@material-ui/core/Dialog'
import DialogActions from '@material-ui/core/DialogActions'
import DialogContent from '@material-ui/core/DialogContent'
import DialogContentText from '@material-ui/core/DialogContentText'
import DialogTitle from '@material-ui/core/DialogTitle'
import { FormValidator, FormField } from '../classes/FormField'
import { Button } from '@material-ui/core'
import { BehaviorSubject } from 'rxjs'
import { FormManager } from '../classes/FormManager'
import { TextFieldWithErrors } from '../components/TextFieldWithError'
import { makeStyles, Theme } from '@material-ui/core/styles'

export interface TextFieldData {
    name: string
    type: string
    validators: FormValidator[]
}

export interface ModalProps {
    open: boolean
    onClose: Function
}

const useStyles = makeStyles((theme: Theme) => ({
    field: {
        marginTop: theme.spacing(2)
    }
}))

export const createFormModal = (
    title: string,
    description: string,
    url: string,
    fields: TextFieldData[],
    onSubmit?: (data: unknown) => void
) => {
    const formFields = fields.map(
        field =>
            new FormField(
                field.name,
                new BehaviorSubject(''),
                new BehaviorSubject({
                    passing: true
                }),
                field.validators
            )
    )

    const formManager = new FormManager(formFields)

    return (props: ModalProps) => {
        const handleClose = (event: unknown) => {
            props.onClose(event)
        }

        const classes = useStyles()

        const textFields = fields.map((field, index) => {
            const fieldObject = formFields[index]

            return (
                <TextFieldWithErrors
                    className={classes.field}
                    name={field.name}
                    type={field.type}
                    input={fieldObject.input}
                    output={fieldObject.output}
                    key={index}
                />
            )
        })

        return (
            <Dialog
                fullWidth={true}
                maxWidth="sm"
                open={props.open}
                aria-labelledby={title}
                onClose={handleClose}
            >
                <DialogTitle id={title}>{title}</DialogTitle>
                <DialogContent>
                    <DialogContentText>{description}</DialogContentText>
                    {textFields}
                </DialogContent>
                <DialogActions>
                    <Button onClick={handleClose} color="primary">
                        Cancel
                    </Button>
                    <Button
                        variant="contained"
                        onClick={event => {
                            if (formManager.validate()) {
                                formManager.submit(url).then(data => {
                                    handleClose(event)

                                    if (onSubmit) onSubmit(data)
                                })
                            }
                        }}
                        color="primary"
                    >
                        Submit
                    </Button>
                </DialogActions>
            </Dialog>
        )
    }
}
