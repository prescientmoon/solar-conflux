import { createValidator } from '../../../common/dom/forms/helpers/createValidator'

export const alphaNumericValidator = createValidator({
    regex: /^[a-zA-Z0-9_]*$/,
    message: 'Must only contain alpha-numeric characters or underscores.'
})
