import Joi from '@hapi/joi'
import { email, name, password } from './authFields'

export const SignupBodySchema = Joi.object({
    name,
    password,
    email
}).required()

export type SignupBody = Joi.extractType<typeof SignupBodySchema>
