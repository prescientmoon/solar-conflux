import { createSchema, Type, typedModel, ExtractDoc } from "ts-mongoose"

const SessionDataSchema = createSchema({
    token: Type.string(),
    data: Type.string()
})

export const SessionData = typedModel("SessionData", SessionDataSchema)
export type SessionDataDoc = ExtractDoc<typeof SessionDataSchema>;