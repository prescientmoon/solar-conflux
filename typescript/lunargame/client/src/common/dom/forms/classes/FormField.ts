import { Subscription, BehaviorSubject } from 'rxjs'

export interface FormFieldSnapshot {
    passing: boolean
    errorMessage?: string
}

export interface FormValidator {
    validate: (input: string) => FormFieldSnapshot
}

export class FormField {
    private subscription: Subscription

    public constructor(
        public name: string,
        public input: BehaviorSubject<string>,
        public output: BehaviorSubject<FormFieldSnapshot>,
        private validators: FormValidator[]
    ) {
        this.subscription = this.input.subscribe((text: string) => {
            for (const validator of this.validators) {
                const result = validator.validate(text)

                if (!result.passing) {
                    return this.output.next(result)
                }
            }

            this.output.next({
                passing: true
            })
        })
    }

    public dispose() {
        if (this.subscription) {
            this.subscription.unsubscribe()
        }
    }

    public passes() {
        return this.output.value.passing
    }

    public get value() {
        return this.input.value
    }
}
