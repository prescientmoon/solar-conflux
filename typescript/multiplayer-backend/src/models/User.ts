import { createSchema, Type, typedModel, ExtractDoc } from "ts-mongoose"

const UserSchema = createSchema({
    name: Type.string(),
    email: Type.string(),
    photo: Type.string(),
    friends: Type.array().of(Type.string()),
    description: Type.optionalString(),
    uid: Type.string()
})

export const User = typedModel("User", UserSchema)
export type UserDoc = ExtractDoc<typeof UserSchema>;