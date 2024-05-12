import { FormField } from './FormField'
import { BaseServer } from '../../../../modules/network/classes/BaseServer'

const server = new BaseServer()

export class FormManager {
    constructor(public fields: FormField[]) {}

    public collect() {
        const data: Record<string, string> = {}

        for (const { name, value } of this.fields) {
            data[name] = value
        }

        return data
    }

    public async submit(url: string) {
        if (this.validate()) {
            return server.request(url, 'POST', this.collect())
        }
    }

    public validate() {
        for (const field of this.fields) {
            if (!field.passes()) return false
        }

        return true
    }

    public dispose() {
        for (const field of this.fields) {
            field.dispose()
        }

        return this
    }
}
