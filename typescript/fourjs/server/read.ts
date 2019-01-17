const fs = require('fs');

function read(url:string):Promise<string>{
    return new Promise((resolve,reject) => {
        fs.readFile(`./${url}`,"utf8",(err,data) => {
            if (err) reject(err);
            resolve(data);
        });
    });
}

export {read};
