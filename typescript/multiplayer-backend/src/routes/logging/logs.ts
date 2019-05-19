import { Router, Response, Request } from "express"
import { shortLogger } from "./shortLog";
import { reddirect } from "../../middleware/reddirect";

const router = Router()

const getShortLogs = (req: Request, res: Response) => {
    res.json(shortLogger.logs)
}

router.get("/", getShortLogs)
router.use("/*", reddirect("/logs"))

export const logs = router