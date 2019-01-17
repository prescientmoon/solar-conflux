"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const read_1 = require("../read");
class ShaderSender {
    constructor(app, dir) {
        this.app = app;
        this.dir = dir;
    }
    async listen(data) {
        for (let i in data) {
            const text = await read_1.read(`./${data[i]}`);
            this.app.get(`/${i}`, (req, res) => {
                res.json({ text });
            });
        }
    }
}
exports.ShaderSender = ShaderSender;
