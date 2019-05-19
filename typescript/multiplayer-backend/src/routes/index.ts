import { auth, token } from "./auth"
import { logs } from "./logging"
import { Router } from "express";

export const routes:{[key:string]:Router} = {
    auth,
    logs,
    token
}
