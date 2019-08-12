import Joi from '@hapi/joi'
import { email, password } from './authFields'

export const LoginBodySchema = Joi.object({
    email,
    password
}).required()

export type LoginBody = Joi.extractType<typeof LoginBodySchema>
