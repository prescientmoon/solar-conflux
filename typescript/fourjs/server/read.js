"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require('fs');
function read(url) {
    return new Promise((resolve, reject) => {
        fs.readFile(`./${url}`, "utf8", (err, data) => {
            if (err)
                reject(err);
            resolve(data);
        });
    });
}
exports.read = read;
