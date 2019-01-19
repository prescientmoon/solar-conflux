const express = require('express');
const http = require('http');

import {Translucid} from "./Translucid";

interface Server{
    app:any,
    server:any,
    connect:Promise<any>
}

function QuickServer(port:number):any{
    const app = express();
    const server = http.Server(app);
    const translucid = new Translucid(app);
    return {
        express,app,http,server,translucid,
        connect:new Promise((resolve,reject) => {
            server.listen(port,() => {
                resolve("Connected");
            });
        })
    }
}

export {QuickServer};
