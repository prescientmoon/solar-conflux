import { createSchema, Type, typedModel, ExtractDoc } from "ts-mongoose"

const PasswordSchema = createSchema({
    uid: Type.string(),
    password: Type.string()
})

export const Passsord = typedModel("Password", PasswordSchema)
export type PasswordDoc = ExtractDoc<typeof PasswordSchema>;