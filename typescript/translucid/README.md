# Translucid
A simple node.js library to bind files to requests

# Get started:

First you need to install the package:
```
npm install translucid --save
```

Then, you need to include it in your project:
```
const trans = require("translucid");
```

To create your first server, call ".QuickServer(port)":
```
trans.QuickServer(8000);
```

The ".QuickServer" method return many useful objects:
```
const {app,server,connect,translucid} = trans.QuickServer(8000);
```
"Connect" is a promise that resolves when the server starts listening to the port:
```
connect.then(() => {
    console.log("Listening on port 8000!");
});
```
The translucid object can be used to make file bindings:
```
translucid.bind("/","client/index.html",true,["myId"]);
```
Then you can add middleware like this:
```
translucid.use({
    name:"my middleware",
    keys:["myid"],
    run:(prev,req,res,next) => {
        //prev is the data that is going to be sent to the client
        //(it might for example be the index.html file);
        next(`${prev} <br/> string added by a middleware`);
    }
});
```
