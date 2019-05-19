import { Router, Response, Request } from "express"
import { randomBytes } from "crypto"
import { SessionData,SessionDataDoc } from "../../models/SessionData";

const router = Router()


const getToken = async (req: Request, res: Response) => {
    //generate token
    const token = randomBytes(16).toString("hex")
    
    //save token into db
    const data = new SessionData({
        token,
        data:"{}"
    } as SessionDataDoc)

    await data.save()

    res.json({
        succes:true,
        data:{
            token
        }
    })
}

router.get("/", getToken)

export const token = router