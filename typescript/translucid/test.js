"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const app_1 = require("./app");
const { app, translucid } = app_1.QuickServer(8000);
translucid.bind("/", "index.html");
translucid.bind("/test", "read.js", true, ["class"]);
translucid.use({
    name: "wow, a midleware",
    run: (prev, req, res, next) => {
        next("sent by a midleware");
    },
    keys: ["class"]
});
