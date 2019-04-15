import { Router, Response, Request } from "express"
import { shortLogger } from "./shortLog";
import { reddirect } from "../../middleware/reddirect";
import { allowWhitelist } from "../../middleware/corsErrorMiddleware";

const router = Router()

const getShortLogs = (req: Request, res: Response) => {
    res.json(shortLogger.logs)
}

router.use(...allowWhitelist())
router.get("/", getShortLogs)
router.use("/*", reddirect("/logs"))

export { router }