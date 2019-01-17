import {read} from "../read"
class ShaderSender{
    constructor(public app,public dir:String){}
    async listen(data:any){
        for (let i in data){
            const text = await read(`./${data[i]}`);
            this.app.get(`/${i}`,(req,res) => {
                res.json({text});
            });
        }
    }
}

export {ShaderSender};
