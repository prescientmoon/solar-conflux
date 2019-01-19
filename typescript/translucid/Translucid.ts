import {read} from "./read";

interface Midleware{
    name:string;
    run:Function;
    keys:Array<string>;
}

function containsAny(array:Array<string>,keys:Array<string>):boolean{
    for (let  i = 0;i < keys.length;i++){
        if (array.indexOf(keys[i]) != -1) return true;
    }
    return false;
}

class Translucid{
    midleware:Array<Midleware> = [];
    constructor(public app){
    }
    use(obj:Midleware):void{
        this.midleware.push(obj);
    }
    bind(path:string="/",filepath:string="",
        text:boolean=false,classes:Array<string>=[]):void{
        this.app.get(path,async (req,res) => {
            if (!text){
                res.sendFile(`${__dirname}/${filepath}`);
            }
            else{
                const readResults = await read(filepath);

                const toRun = [];
                for (let i of this.midleware){
                    if (containsAny(classes,i.keys)){
                        toRun.push(i.run);
                    }
                }
                const decorated = [];

                const expressArgs = [req,res];

                for (let  i = 0;i < toRun.length;i++){
                    decorated.push((prev) => {
                        toRun[i](prev,...expressArgs,decorated[i + 1]);
                    });
                }

                decorated.push((prev) => {
                    res.send(prev + "<br/>Sent from the last iteration");
                });

                decorated[0](readResults);
            }
        });
    }
}

export {Translucid};
