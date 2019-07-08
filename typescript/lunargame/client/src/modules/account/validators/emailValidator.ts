import { createValidator } from '../../../common/dom/forms/helpers/createValidator'

export const emailValidator = createValidator({
    regex: /^(([^<>()\[\]\.,;:\s@\"]+(\.[^<>()\[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()[\]\.,;:\s@\"]+\.)+[^<>()[\]\.,;:\s@\"]{2,})$/,
    message: 'Must be a valid email'
})
