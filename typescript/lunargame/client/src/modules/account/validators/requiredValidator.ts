import { createValidator } from '../../../common/dom/forms/helpers/createValidator'

export const requiredValidator = createValidator({
    regex: /^.{1,}$/,
    message: 'Field is required'
})
