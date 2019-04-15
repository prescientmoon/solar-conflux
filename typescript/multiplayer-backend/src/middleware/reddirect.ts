import { Request, Response } from "express"

/**
 * boilerplate for reddirecting
 * @param url the url to reddirect to 
 * @returns the middleware
 */
export const reddirect = (url: string) =>
    (req: Request, res: Response, next: any) => {
        res.redirect(url)
        next()
    }
