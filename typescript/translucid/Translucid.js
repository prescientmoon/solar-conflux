"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const express = require('express');
const read_1 = require("./read");
function containsAny(array, keys) {
    for (let i = 0; i < keys.length; i++) {
        if (array.indexOf(keys[i]) != -1)
            return true;
    }
    return false;
}
class Translucid {
    constructor(app) {
        this.app = app;
        this.middleware = [];
    }
    use(obj) {
        this.middleware.push(obj);
    }
    public(path = "") {
        this.app.use(`/${path}`, express.static(`${__dirname}/../../${path}`));
    }
    async bindJSON(path) {
        const json = await read_1.read(path);
        const object = JSON.parse(json);
        for (let i in object) {
            const classes = object[i].classes || [];
            this.bind(i, object[i].file, classes);
            //gotta comment this later
            // console.log(`Binded room with name ${i} and path ${object[i].file} with classes ${classes}`)
        }
    }
    bind(path = "/", filepath = "", classes = []) {
        this.app.get(path, async (req, res) => {
            const readResults = await read_1.read(filepath);
            const toRun = [];
            for (let i of this.middleware) {
                if (containsAny(classes, i.keys)) {
                    toRun.push(i.run);
                }
            }
            const decorated = [];
            const expressArgs = [req, res];
            for (let i = 0; i < toRun.length; i++) {
                decorated.push((prev) => {
                    toRun[i](prev, ...expressArgs, decorated[i + 1]);
                });
            }
            decorated.push((prev, req, res) => {
                console.log(`${__dirname}/../../${filepath}`);
                // res.contentType(`${__dirname}/../../${filepath}`);
                res.send(prev);
            });
            decorated[0](readResults);
        });
    }
}
exports.Translucid = Translucid;
