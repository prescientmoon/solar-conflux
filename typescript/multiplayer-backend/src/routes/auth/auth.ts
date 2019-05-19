import { Router, Response, Request } from "express"
import { reddirect } from "../../middleware/reddirect";
import * as validator from "express-validator"
import { generateUid, encodePassword } from "../../services/auth/signup";
import { savePassword } from "../../services/auth/signup";
import { corsErrorFixer } from "../../middleware/corsErrorMiddleware";
import { config } from "dotenv"
import { compare } from "bcryptjs";
import { User, UserDoc } from "../../models/User";
import { Passsord } from "../../models/Password";

//extract env variables
config()

const router = Router()

router.get("/test", (req, res) => {
    res.send("just a test")
})

const loginHtml = (req: Request, res: Response) => {
    res.send(`
        <form action="/auth/login" method=post>
                <div>
                    <label for=email>email</label>
                    <input type=text id=email name=email>
                </div>
                <div>
                    <label for=password>password</label>
                    <input type=password id=password name=password>
                </div>
                <div>
                    <label for=name>name</label>
                    <input type=name id=name name=name>
                </div>
                <button type=submit onclick="
                    alert('click')
                    fetch('/',{
                        headers: {
                            authorization: 'do u see this?'
                        }
                    })">Submit</button>
            </form>
    `)
}

const sayHello = (req: Request, res: Response) => {
    res.send(`
        <form action="/auth" method=post>
            <div>
                <label for=email>email</label>
                <input type=text id=email name=email>
            </div>
            <div>
                <label for=password>password</label>
                <input type=password id=password name=password>
            </div>
            <div>
                <label for=name>name</label>
                <input type=name id=name name=name>
            </div>
            <button type=submit>Submit</button>
        </form>
    `)
}

const authHandler = async (req: Request, res: Response) => {
    try {
        //if already logged in return the uid
        if (req.session.uid)
            return res.redirect("auth/account")

        //get data from body
        const { password, email, name } = req.body

        //check if the email isnt used
        if (await User.findOne({ name }))
            res.redirect("login")

        //validate
        req.check("email", "email isnt valid").isEmail()
        const errors = req.validationErrors()

        if (errors)
            res.json({ error: `${req.body.email} is not a valid email` })

        //generate an uid
        const uid = await generateUid()

        const user = new User({
            email,
            friends: [],
            name,
            photo: process.env.DEFAULTPROFILEPIC,
            uid
        } as UserDoc) //used for my editor to help me

        encodePassword(password)

        //save the password and the user in the db
        await Promise.all([
            encodePassword(password).then(result => savePassword(uid, result)),
            user.save()
        ])

        //save things in session
        req.session.uid = uid

        //save in the session
        req.session.save(() => { })

        //send uid back
        res.json({
            succes: true,
            data: {
                uid
            }
        }).status(200)
    }
    catch (errors) {
        //send erros to clinet
        res.json({ errors })
    }
}

const account = async (req: Request, res: Response) => {
    try {
        if (!req.session.uid)
            res.json({
                succes: false,
                errors: ["uid doesnt exist"]
            }).status(203)

        res.json({
            succes: true,
            data: {
                uid: req.session.uid
            }
        })
    }
    catch (errors) {
        //send erros to clinet
        res.json({ errors })
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

router.use("*", validator(), corsErrorFixer)

router
    .get("/", sayHello)
    .post("/", authHandler)
    .get("/login", loginHtml)
    .post("/login", login)
    .get("/account", account)
    .get("/logout", logout)
    .get("/uid", (req, res) => {
        res.send(req.session.uid)
    })
    .get("/test/uid", async (req, res) => {
        res.send(await generateUid())
    })

router.use("/*", reddirect("/auth"))

export const auth = router