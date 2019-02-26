import { inputEmitter } from "./inputEmitter.js";
import * as axios from "./os.js";
inputEmitter.on("submit", (data) => {
    axios.run(data);
}).on("up", (data) => {
    axios.back();
}).on("blue", (e) => {
    axios.addBluePortal(e);
}).on("red", (e) => {
    axios.addRedPortal(e);
}).on("start", axios.start);
