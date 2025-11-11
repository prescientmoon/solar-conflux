import { EventEmitter } from "./emitter.js";
import * as keyCode from "key-code";
import { fadeOut } from "./animations.js";
const terminal = document.getElementById("terminal");
const input = document.getElementById("terminal-input");
const canvas = document.getElementById("canvas");
const inputEmitter = new EventEmitter();
let startedMusic = false;
let music = new Audio("assets/music.ogg");
window.onresize = (e) => inputEmitter.emit("resize", e);
terminal.addEventListener("change", (e) => {
    inputEmitter.emit("change", e);
});
document.getElementById("logo").addEventListener("drag", (e) => e.preventDefault());
document.addEventListener("keydown", (e) => {
    if (input == document.activeElement && e.keyCode == keyCode.ENTER) {
        inputEmitter.emit("submit", input.value);
        input.value = "";
    }
    else if (e.keyCode == keyCode.UP && input == document.activeElement)
        inputEmitter.emit("up", e);
    else if (e.keyCode == keyCode.S && input != document.activeElement)
        inputEmitter.emit("start", e);
    else if (e.keyCode == keyCode.T && input != document.activeElement)
        inputEmitter.emit("toggle", e);
    else if (e.keyCode == keyCode.Q && input != document.activeElement)
        inputEmitter.emit("effectTest", e);
});
canvas.addEventListener("click", (e) => {
    inputEmitter.emit("blue", [e.clientX - canvas.offsetLeft, e.clientY - canvas.clientTop]);
});
canvas.addEventListener("contextmenu", (e) => {
    inputEmitter.emit("red", [e.clientX - canvas.offsetLeft, e.clientY - canvas.clientTop]);
});
document.getElementById("play").addEventListener("click", (e) => {
    e.preventDefault();
    fadeOut(document.getElementsByClassName("menu")[0], 0.05);
    inputEmitter.emit("start", e);
});
export { inputEmitter };
