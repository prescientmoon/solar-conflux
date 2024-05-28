"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const express = require("express");
const app = express();
const http = require("http").Server(app);
app.get("/", (req, res) => {
    res.sendFile(`${__dirname}/public/index.html`);
});
app.use("/public", express.static(__dirname + "/public"));
const port = 8000;
http.listen(port, () => {
    console.log(`>>Listeing on port ${port}`);
});
const sender_1 = require("./server/four/sender");
const sender = new sender_1.ShaderSender(app, __dirname);
sender.listen({
    vertex: "shaders/vertex.glsl",
    fragment: "shaders/fragment.glsl"
});
