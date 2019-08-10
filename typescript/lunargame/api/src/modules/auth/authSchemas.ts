import Joi from 'joi'

export const token = Joi.string().required()
export const field = token.alphanum()

// not merging them cause i'll add more to the password in the future
export const name = field.max(30).min(3)
export const password = field.min(3).max(30)

export const email = Joi.string()
    .required()
    .email()
    .min(3)
    .max(30)

export const hasName = Joi.object({ name })
export const hasEmail = Joi.object({ email })
export const hasVerificationToken = Joi.object({ token })

export const loginSchema = Joi.object({
    email,
    password
})

export const createUserSchema = Joi.object({
    name,
    email,
    password
})
