import * as bcrypt from "bcryptjs"
import { promisify } from "util";
import { randomBytes } from "crypto";
import { database } from "../db/firestore";
import { Passsord, PasswordDoc } from "../../models/Password";
import { User } from "../../models/User";

//promisify functions
const hash = promisify(bcrypt.hash)


export async function generateUid(): Promise<string> {
    //generate uid
    const uid = randomBytes(16).toString("hex")

    //check if document exists
    const doc = await User.findOne({ uid })

    //if theres already an user with the same id, regenerate
    if (doc)
        return generateUid()

    //then return the uid
    return uid
}

export function encodePassword(password: string): Promise<string> {
    return hash(password, 10)
}

export function savePassword(uid: string, password: string) {
    const passwordInstance = new Passsord({
        password, uid
    } as PasswordDoc)

    return passwordInstance.save()
}

export async function getPassword(uid: string) {
    //get doc
    const doc = await database.doc(`passwords/${uid}`).get()

    //return result
    return doc.data()
}
