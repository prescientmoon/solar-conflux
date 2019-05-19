import { Request, Response } from "express"
import { whiteList } from "../../config"
import { includes } from "lodash"

const urls = whiteList.map(urlObject => urlObject.url)

//TODO: remove from here
const defaultMethods = "GET, POST, OPTIONS, PUT, PATCH, DELETE"

/**
 * used to fix the cors errors things
 * @param url the url to allow
 * @param methods the methods to allow the url to do
 */
export const corsErrorFixer = (req: Request, res: Response, next: any) => {
    const host = req.get("host")
    const methods: any = whiteList.find(value => value.url == host) || { methods: undefined }

    if (!includes(urls, host))
        return next()

    res.setHeader('Access-Control-Allow-Origin', host) // Website you wish to allow to connect 
    res.setHeader('Access-Control-Allow-Methods', methods.methods || defaultMethods) //// Request methods you wish to allow
    res.setHeader('Access-Control-Allow-Headers', 'X-Requested-With,content-type') // Request headers you wish to allow

    // Pass to next layer of middleware
    next();
}