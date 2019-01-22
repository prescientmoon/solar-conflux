const express = require('express');
import {read} from "./read";

interface Middleware{
    name:string;
    keys:Array<string>;
    run:Function;
}

function containsAny(array:Array<string>, keys:Array<string>):boolean{
    for (let i = 0; i < keys.length; i++) {
        if (array.indexOf(keys[i]) != -1)
            return true;
    }
    return false;
}
class Translucid {
    middleware:Array<Middleware> = [];

    constructor(public app) {}
    use(obj:Middleware):void{
        this.middleware.push(obj);
    }
    public(path:string = ""):void{
        this.app.use(`/${path}`, express.static(`${__dirname}/../../${path}`));
    }
    async bindJSON(path:string):Promise<void>{
        const json = await read(path);
        const object = JSON.parse(json);

        for (let i in object){
            const classes = object[i].classes || [];
            this.bind(i,object[i].file,classes);
            //gotta comment this later
            // console.log(`Binded room with name ${i} and path ${object[i].file} with classes ${classes}`)
        }
    }
    bind(path:string = "/", filepath:string = "", classes:Array<string> = []):void{
        this.app.get(path, async (req, res)=> {
            const readResults = await read(filepath);
            const toRun:Array<Function> = [];

            for (let i of this.middleware){
                if (containsAny(classes, i.keys)) {
                    toRun.push(i.run);
                }
            }

            const decorated:Array<Function> = [];
            const expressArgs = [req, res];

            for (let i = 0; i < toRun.length; i++) {
                decorated.push((prev:any):void => {
                    toRun[i](prev, ...expressArgs, decorated[i + 1]);
                });
            }
            decorated.push((prev:any):void => {
                console.log(`${__dirname}/../../${filepath}`);
                // res.contentType(`${__dirname}/../../${filepath}`);
                res.send(prev);
            });

            decorated[0](readResults);
        });
    }
}
export {Translucid};
