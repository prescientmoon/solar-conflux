import { Response } from "express";

export const respond = (res:Response, succes = true, data: any = {}, 
    errors:any[] = [], status = 200) => {
    res.json({
        succes,
        data,
        errors
    }).status(status)
}