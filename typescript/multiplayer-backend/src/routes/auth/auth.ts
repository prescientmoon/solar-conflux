import { Router, Response, Request, urlencoded } from "express"
import { reddirect } from "../../middleware/reddirect";
import { allowWhitelist } from "../../middleware/corsErrorMiddleware";
import * as sessions from "express-session"
import * as validator from "express-validator"

const router = Router()

const sayHello = (req: Request, res: Response) => {
    res.send(`
        <form action="/auth" method=post>
            <div>
                <label for=email>email</label>
                <input type=text id=email name=email>
            </div>
            <button type=submit>Submit</button>
        </form>
    `)
}

const auth = (req: Request, res: Response, next: any) => {
    //validate
    req.check("email", "email isnt valid").isEmail()
    const errors = req.validationErrors()

    //if we have erros mark it
    if (errors)
        req.session.errors = errors

    //reddirect to page
    res.send(errors || "Succes!!!")
}

router.use(...allowWhitelist())
router.use("*", urlencoded({ extended: true }), validator(), sessions({
    secret: process.env.SESSION_SECRET,
    saveUninitialized: false,
    resave: false
}))

router.get("/", sayHello)
router.post("/", auth)

router.use("/*", reddirect("/auth"))

export { router }