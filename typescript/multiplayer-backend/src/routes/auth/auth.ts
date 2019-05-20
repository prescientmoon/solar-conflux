import { Router, Response, Request } from "express"
import * as validator from "express-validator"
import { generateUid, encodePassword } from "../../services/auth/signup";
import { savePassword } from "../../services/auth/signup";
import { config } from "dotenv"
import { compare } from "bcryptjs";
import { User, UserDoc } from "../../models/User";
import { Passsord } from "../../models/Password";
import { preventGlobalErrors } from "../../common/preventGlobalError";
import { respond } from "../../common/respond";

//extract env variables
config()

const router = Router()

const authHandler = preventGlobalErrors(async (req: Request, res: Response) => {
    //if already logged in return the uid
    if (req.session.uid)
        return respond(res, true, { uid: req.session.uid })

    //get data from body
    const { password, email, name } = req.body

    //check if the email isnt used
    if (await User.findOne({ email }))
        return respond(res, false, { email }, ["email is already used"], 400)

    //validate
    req.check("email", "email isnt valid").isEmail()
    const errors = req.validationErrors() as any[]

    if (errors)
        return respond(res, false, {}, errors, 400)

    //generate an uid
    const uid = await generateUid()

    const user = new User({
        email, name, uid,
        friends: [],
        photo: process.env.DEFAULTPROFILEPIC
    } as UserDoc) //used for my editor to help me

    //save things in session
    req.session.uid = uid
    req.session.save(() => { })

    await Promise.all([
        encodePassword(password).then(result => savePassword(uid, result)),
        user.save()
    ])

    //send uid back
    return respond(res,true,{ uid })
})

const account = async (req: Request, res: Response) => {
    try {
        if (!req.session.uid)
            res.json({
                succes: false,
                data: {},
                errors: ["uid doesnt exist"]
            }).status(203)

        res.json({
            succes: true,
            data: {
                uid: req.session.uid
            }
        })
    }
    catch (err) {
        //send erros to clinet
        res.json({ succes: false, errors: [err] })
    }
}

const login = async (req: Request, res: Response) => {
    try {
        //check if we have an email or an username, and if we are note already logged in
        if (req.session.uid)
            return res.redirect("account")

        let type = "name"

        if (req.body.email)
            type = "email"

        const { password } = req.body
        const data = req.body[(type == "name") ? "name" : type]

        const doc = await User.findOne({ [data]: req.body[data] })
        const uid = doc.uid

        //check if the password is good
        const passwordHash = await Passsord.findOne({ uid })
        const match = await compare(password, passwordHash.password)

        //return result
        if (!match)
            return res.json({
                succes: false,
                errors: ["wrong password"],
                data: {
                    uid,
                    hash: passwordHash.password,
                    password,
                    [data]: req.body[data]
                }
            }).status(400)

        //update the session ond save it
        req.session.uid = uid
        req.session.save(() => { })

        //send it to the clinet
        return res.json({ uid })
    }
    catch (errors) {
        return res.json({ errors })
    }
}


const logout = async (req: Request, res: Response) => {
    //clear the uid
    req.session.uid = undefined

    //save it to the db
    req.session.save(() => { })

    res.send({ succes: true })
}

router.use("*", validator())

router
    .post("/", authHandler)
    .post("/login", login)
    .get("/account", account)
    .get("/logout", logout)
    .get("/uid", (req, res) => {
        res.send(req.session.uid)
    })
    .get("/test/uid", async (req, res) => {
        res.send(await generateUid())
    })

export const auth = router