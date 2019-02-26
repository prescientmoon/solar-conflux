import { EventEmitter } from "./emitter.js";
import * as keyCode from "key-code";
const terminal = document.getElementById("terminal");
const input = document.getElementById("terminal-input");
const canvas = document.getElementById("canvas");
const inputEmitter = new EventEmitter();
terminal.addEventListener("change", (e) => {
    inputEmitter.emit("change", e);
});
document.addEventListener("keydown", (e) => {
    if (input == document.activeElement && e.keyCode == keyCode.ENTER) {
        inputEmitter.emit("submit", input.value);
        input.value = "";
    }
    else if (e.keyCode == keyCode.UP)
        inputEmitter.emit("up", e);
    else if (e.keyCode == keyCode.S)
        inputEmitter.emit("start", e);
});
canvas.addEventListener("click", (e) => {
    inputEmitter.emit("blue", [e.clientX - canvas.offsetLeft, e.clientY - canvas.clientTop]);
});
canvas.addEventListener("contextmenu", (e) => {
    inputEmitter.emit("red", [e.clientX - canvas.offsetLeft, e.clientY - canvas.clientTop]);
});
export { inputEmitter };
