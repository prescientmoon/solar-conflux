import { Request, Response } from "express";

export const preventGlobalErrors = (func: (req: Request, res: Response) => any) => {
    return async (req: Request, res: Response) => {
        try {
            await func(req,res)
        }
        catch (err) {
            res.json({
                succes: false,
                errors: [err]
            }).status(400)
        }
    }
}