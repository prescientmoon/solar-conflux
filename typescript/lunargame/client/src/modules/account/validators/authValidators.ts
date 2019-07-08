import { fieldLengthValidator } from './fieldLength'
import { emailValidator } from './emailValidator'
import { requiredValidator } from './requiredValidator'
import { alphaNumericValidator } from './alphaNumeric'

export const usernameValidatorList = () => [
    new requiredValidator(),
    new fieldLengthValidator(),
    new alphaNumericValidator()
]
export const emailValidatorList = () => [
    new requiredValidator(),
    new fieldLengthValidator(),
    new emailValidator()
]
export const passwordValidatorList = () => [
    new requiredValidator(),
    new fieldLengthValidator(),
    new alphaNumericValidator()
]
