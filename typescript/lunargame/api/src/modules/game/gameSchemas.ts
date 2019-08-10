import Joi from 'joi'

export const pageSize = Joi.number()
    .required()
    .max(50)
    .min(3)

export const page = Joi.number().required()

export const chunkSchema = Joi.object({
    pageSize,
    page
})
