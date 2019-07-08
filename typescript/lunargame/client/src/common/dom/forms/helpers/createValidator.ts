import { FormValidator, FormFieldSnapshot } from '../classes/FormField'

export interface ValidatorCondition {
    regex: RegExp
    message: string
}

export const createValidator = (
    ...conditions: ValidatorCondition[]
): { new (): FormValidator } =>
    class implements FormValidator {
        public validate(text: string): FormFieldSnapshot {
            for (const condition of conditions) {
                if (!condition.regex.test(text)) {
                    return {
                        passing: false,
                        errorMessage: condition.message
                    }
                }
            }

            return {
                passing: true
            }
        }
    }
