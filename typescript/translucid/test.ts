import {QuickServer,Translucid} from "./app";

const {app,translucid} = QuickServer(8000);
translucid.bind("/","index.html");
translucid.bind("/test","read.js",true,["class"]);
translucid.use({
    name:"wow, a midleware",
    run:(prev,req,res,next) => {
        next("sent by a midleware");
    },
    keys:["class"]
});
