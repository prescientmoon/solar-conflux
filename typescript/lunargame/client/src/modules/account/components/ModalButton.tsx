import React, { useState } from 'react'
import Button from '@material-ui/core/Button'
import { ModalProps } from '../../../common/dom/forms/helpers/createFormModal'

export interface ModalButtonProps {
    modal: (props: ModalProps) => JSX.Element
    children: string
    className?: string
    contained?: boolean
}

export const ModalButton = (props: ModalButtonProps) => {
    const [open, setOpen] = useState(false)

    return (
        <>
            <Button
                className={props.className}
                variant={props.contained ? 'contained' : 'text'}
                onClick={() => setOpen(true)}
            >
                {props.children}
            </Button>
            <props.modal open={open} onClose={() => setOpen(false)} />
        </>
    )
}
