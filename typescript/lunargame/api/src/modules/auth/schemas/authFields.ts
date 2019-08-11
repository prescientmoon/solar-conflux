import Joi from 'joi'

export const name = Joi.string()
    .alphanum()
    .min(3)
    .max(30)
    .lowercase()
    .required()

export const email = Joi.string()
    .email()
    .min(3)
    .max(30)
    .required()

export const password = Joi.string()
    .min(3)
    .max(50)
    .alphanum()
    .required()
