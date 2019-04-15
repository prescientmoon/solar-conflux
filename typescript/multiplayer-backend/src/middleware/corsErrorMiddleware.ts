import { Request, Response } from "express"
import { whiteList, whiteListUrl } from "../../config"

//TODO: remove from here
const defaultMethods = "GET, POST, OPTIONS, PUT, PATCH, DELETE"

/**
 * used to fix the cors errors things
 * @param url the url to allow
 * @param methods the methods to allow the url to do
 */
export const corsErrorFixer = ({url,methods}:whiteListUrl) => {
    return (req: Request, res: Response, next: any) => {
        res.setHeader('Access-Control-Allow-Origin', url) // Website you wish to allow to connect 
        res.setHeader('Access-Control-Allow-Methods', methods || defaultMethods) //// Request methods you wish to allow
        res.setHeader('Access-Control-Allow-Headers', 'X-Requested-With,content-type') // Request headers you wish to allow

        // Pass to next layer of middleware
        next();
    }
}

/**
 * used to allow all urls on the whitelist
 */
export const allowWhitelist = () => {
    return whiteList.map(value => corsErrorFixer(value))
}

