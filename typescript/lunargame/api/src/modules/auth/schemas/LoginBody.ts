import Joi from 'joi'
import { name, password } from './authFields'

export const LoginBodySchema = Joi.object({
    name,
    password
})
