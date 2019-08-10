import { Singleton } from '@eix/utils'
import { config } from 'dotenv'
import sendgrid from '@sendgrid/mail'

config()

@Singleton
export class EmailManager {
    private email_adress = process.env.EMAIL_ADRESS
    private key = process.env.SENDGRID_API_KEY

    constructor() {
        sendgrid.setApiKey(this.key)
    }

    public send(to: string, subject: string, text: string) {
        return sendgrid.send({
            from: this.email_adress,
            to,
            subject,
            text
        })
    }
}
