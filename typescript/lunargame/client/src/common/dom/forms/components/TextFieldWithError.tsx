import React from 'react'
import TextField from '@material-ui/core/TextField'
import { BehaviorSubject } from 'rxjs'
import { FormFieldSnapshot } from '../classes/FormField'
import { useObservable } from 'rxjs-hooks'

export interface TextFieldWithErrorsProps {
    name: string
    type: string
    input: BehaviorSubject<string>
    output: BehaviorSubject<FormFieldSnapshot>
    className: string
}

const good = '✅'

export const TextFieldWithErrors = (props: TextFieldWithErrorsProps) => {
    const outputSnapshot = useObservable(() => props.output, {
        passing: true,
        errorMessage: good
    })

    return (
        <TextField
            fullWidth
            className={props.className}
            label={props.name}
            type={props.type}
            error={!outputSnapshot.passing}
            helperText={
                outputSnapshot.passing ? good : outputSnapshot.errorMessage
            }
            onChange={event => {
                props.input.next(event.target.value)
            }}
        />
    )
}
