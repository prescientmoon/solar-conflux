import { createValidator } from '../helpers/createValidator'

export const lengthValidator = (min: number, max: number) =>
    createValidator({
        regex: new RegExp(`^.{${min},${max}}$`),
        message: `Must contain between ${min} and ${max} characters.`
    })
