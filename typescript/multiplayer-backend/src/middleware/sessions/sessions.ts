import { Response, Request } from "express";
import { SessionDataDoc, SessionData } from "../../models/SessionData"

const getToken = (req: Request) => {
    if (req.headers.authorization && req.headers.authorization.split(' ')[0] === 'Bearer') { // Authorization: Bearer g1jipjgi1ifjioj
        // Handle token presented as a Bearer token in the Authorization header
        return req.headers.authorization.split(' ')[1];
    } else if (req.query && req.query.token) {
        // Handle token presented as URI param
        return req.query.token;
    } else if (req.cookies && req.cookies.token) {
        // Handle token presented as a cookie parameter
        return req.cookies.token;
    }

    // If we return null, we couldn't find a token.
    // In this case, the JWT middleware will return a 401 (unauthorized) to the client for this request
    return null;
}

export const sessionMiddleware = async (req: Request, res: Response, next: Function) => {
    const token = getToken(req)

    //if we are trying to get an token, allow this
    if (req.path === "/token")
        return next()

    //if we dont have any token
    if (!token)
        return res.json({ succes: false }).status(400)

    //try searching for the object in the database
    const result = await SessionData.findOne({ token })

    if (!result)
        return res.json({ succes: false }).status(400)

    const data = JSON.parse(result.data)

    if (!req.session)
        //@ts-ignore
        req.session = {}

    for (let i in data)
        req.session[i] = data[i]

    req.session.save = async () => {
        const toSave:any = {}

        for (let i in req.session) {
            if (i == "save") continue

            toSave[i] = req.session[i]
        }

        const data:string = JSON.stringify(toSave)
        
        return await result.updateOne({
            token,
            data
        })
    } 

    next()
}