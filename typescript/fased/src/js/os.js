import { print } from "./print";
import { Planet } from "./planet";
import { length, tau } from "./math";
import * as loop from "mainloop.js";
import { WormHole } from "./WormHole";
let initialSystem = {
    planets: [
        [20, 100, [200, 200], [0, 0], true],
        [10, 10, [200, 100], [1, 0], false]
    ],
    G: 1
};
let planets = [];
let wormHoles = [];
const terminal = document.getElementById("terminal");
const input = document.getElementById("terminal-input");
const canvas = document.getElementById("canvas");
const ctx = canvas.getContext("2d");
canvas.height = window.innerHeight;
canvas.width = window.innerWidth;
const original_os_name = "axios";
const commandHistory = [];
let os_name = original_os_name;
let currentCommand = -1;
let tempPortals = [null, null];
let portalLimit = 300;
const loadSystem = (data) => {
    planets = [];
    wormHoles = [];
    tempPortals = [];
    memory.G = data.G;
    print("<span class'red'>Reseting G...</span>");
    for (let i of data.planets) {
        print(`<span class="blue">Creating the planet:</span> ${i}...`);
        const newplanet = new Planet(i[0], i[1], i[2], i[4]);
        newplanet.speed = [i[3][0], i[3][1]];
        print(`<span class="blue">Loading the planet</span> ${i} <span class="blue">into the universe...</span>`);
        planets.push(newplanet);
    }
    print("<span class='green'>Universe succesfully reseted.</span>");
};
const memory = {
    warp: 0.05,
    G: 1
};
const commands = {
    "set": (key, value, path) => {
        print(path);
        memory[key] = value;
        print(memory, false);
    },
    "rename": (name, path) => {
        if (!path) {
            commands.rename(original_os_name, name);
            return;
        }
        const old_name = os_name;
        os_name = name;
        print(path);
        print(`<span class="green">${old_name}</span> succesfully renamed to <span class="red">${os_name}</span>`);
    },
    "print": (key, path) => {
        if (!path) {
            commands.print("*", key);
            return;
        }
        print(path);
        if (key == "*" || key === undefined || key === null || key === "")
            print(memory, false);
        else if (memory[key] != undefined)
            print(memory[key], false);
        else
            print(`<span class="error"> Error!!! ${key} is undefined </span>`, false);
    },
    "help": (path = "") => {
        print(path);
        print(`<span style="color:red">${os_name} set key value </span> => sets the value of key to value. You can use any variable you defined in your commands by adding the @ prefix.`);
        print(`<span class="example"> Example: </span>
            <br> >   ${os_name} set p print 
            <br> >   ${os_name} set 3 7 
            <br> >   ${os_name} @p 3 
            <br> >   7`);
        print(`<span style="color:red">${os_name} print key? </span> => prints the value of key (or the entire memory if key is  * or if key is not passed)`);
        print(`<span class="example"> Example: </span>
            <br> >   ${os_name} set 3 7 
            <br> >   ${os_name} print 3 
            <br> >   7
            <br> > ${os_name} print
            <br> > {"3":"7"}`);
        print(`<span style="color:red">${os_name} rename name</span> => changes the name of the cli to name`);
        print(`<span class="example"> Example: </span>
            <br> >   ${os_name} rename OwO 
            <br> >   ${os_name} print 3 <span class="comment">//some error</span>
            <br> >   OwO print <span class="comment">//works</span>`);
        print(`<span style="color:red">${os_name} start </span> => starts the universe`);
        print(`<span style="color:red">${os_name} stop </span> => stops the universe`);
        print(`<span style="color:red">${os_name} clear </span> => clears the console`);
    },
    "clear": () => {
        terminal.innerHTML = "";
    },
    "start": (path) => {
        print(path);
        loop.stop().start();
        print(`<span class="green">The universe was succesfully started</span>`);
    },
    "stop": (path) => {
        print(path);
        loop.stop();
        print(`<span class="green">The universe was succesfully sopeed</span>`);
    },
    "restart": (path) => {
        print(path);
        loadSystem(initialSystem);
    }
};
function run(command) {
    const words = command.split(" ");
    commandHistory.push(command);
    currentCommand = commandHistory.length;
    for (let i = 0; i < words.length; i++) {
        if (words[i][0] == "@" && memory[words[i].substr(1)] != undefined)
            words[i] = memory[words[i].substr(1)];
    }
    const path = `<span class="blue"> >>>${words.join(" ")}: </span>`;
    if (command[0] == "/" && command[1] == "/") {
        print(`<span class="comment">//${command.substr(2)}<span>`);
        return;
    }
    if (words[0] == os_name && words.length > 1) {
        const args = [...words];
        args.shift();
        args.shift();
        if (commands[words[1]] != undefined)
            commands[words[1]](...args, path);
        else
            print(`<span class="error">The command <span class="green">${words[1]}</span> is not defined. Run <span class="blue">${os_name}</span> or <span class="blue">${os_name} help</span> to get a list of all commands</span>`);
    }
    else if (words[0] == os_name)
        commands.help(path);
    else
        print(`<span class="error">The command <span class="green">${words.join(" ")}</span> is not defined. Run <span class="blue">${os_name}</span> or <span class="blue">${os_name} help</span> to get a list of all commands</span>`);
}
const back = () => {
    if (currentCommand < 1 || currentCommand - 1 >= commandHistory.length)
        return;
    input.value = commandHistory[--currentCommand];
};
const addBluePortal = (e) => {
    if (!tempPortals[0] || !tempPortals[1]) {
        if (!tempPortals[1] || length(...e, ...tempPortals[1]) < portalLimit)
            tempPortals[0] = e;
        if (tempPortals[0] && tempPortals[1])
            wormHoles[0] = new WormHole(20, tempPortals[0], tempPortals[1]);
    }
    else if (length(...wormHoles[0].end, ...e) < portalLimit)
        wormHoles[0].start = e;
};
const addRedPortal = (e) => {
    if (!tempPortals[0] || !tempPortals[1]) {
        if (!tempPortals[0] || length(...e, ...tempPortals[0]) < portalLimit)
            tempPortals[1] = e;
        if (tempPortals[0] && tempPortals[1])
            wormHoles[0] = new WormHole(20, tempPortals[0], tempPortals[1]);
    }
    else if (length(...wormHoles[0].start, ...e) < portalLimit)
        wormHoles[0].end = e;
};
const applyGravity = (time) => {
    for (let i of planets) {
        for (let j of planets) {
            if (i != j) {
                const distance = length(...i.position, ...j.position);
                const acceleration = time * (i.mass * parseFloat(memory.G.toString())) / (distance ** 2);
                const direction = [
                    acceleration * (j.position[0] - i.position[0]) / distance,
                    acceleration * (j.position[1] - i.position[1]) / distance
                ];
                j.speed = [j.speed[0] - direction[0], j.speed[1] - direction[1]];
            }
        }
    }
};
function move(time) {
    for (let i of planets) {
        if (i.locked)
            continue;
        i.position = i.position.map((val, index) => val + time * i.speed[index]);
    }
}
const drawPortalLimit = (color, position) => {
    let colorWithAlpha;
    if (color == "red")
        colorWithAlpha = "rgba(256,0,0,0.5)";
    else if (color == "blue")
        colorWithAlpha = "rgba(0,0,256,0.5)";
    else
        console.error(`Color: ${color} is not defined`);
    ctx.fillStyle = colorWithAlpha;
    ctx.beginPath();
    ctx.arc(position[0], position[1], portalLimit, 0, tau);
    ctx.fill();
};
const clear = () => {
    ctx.fillStyle = "rgba(0,0,0,0.9)";
    ctx.fillRect(0, 0, 2000, 2000);
};
const draw = () => {
    clear();
    for (let i of planets) {
        ctx.fillStyle = "#ffff00";
        ctx.beginPath();
        ctx.arc(i.position[0], i.position[1], i.radius, 0, tau);
        ctx.fill();
    }
    for (let i of wormHoles) {
        drawPortalLimit("blue", i.start);
        drawPortalLimit("red", i.end);
        ctx.fillStyle = "#0000ff";
        ctx.beginPath();
        ctx.arc(i.start[0], i.start[1], i.radius, 0, tau);
        ctx.fill();
        ctx.fillStyle = "#ff0000";
        ctx.beginPath();
        ctx.arc(i.end[0], i.end[1], i.radius, 0, tau);
        ctx.fill();
    }
    if (tempPortals[0] && !wormHoles[0]) {
        drawPortalLimit("blue", tempPortals[0]);
        ctx.fillStyle = "#0000ff";
        ctx.beginPath();
        ctx.arc(tempPortals[0][0], tempPortals[0][1], 20, 0, tau);
        ctx.fill();
    }
    if (tempPortals[1] && !wormHoles[0]) {
        drawPortalLimit("red", tempPortals[1]);
        ctx.fillStyle = "#ff0000";
        ctx.beginPath();
        ctx.arc(tempPortals[1][0], tempPortals[1][1], 20, 0, tau);
        ctx.fill();
    }
};
const checkTeleportation = () => {
    for (let i of planets) {
        for (let j of wormHoles) {
            if (length(...j.start, ...i.position) < j.radius + i.radius) {
                i.position = j.end.map((val, index) => val + j.start[index] - i.position[index]);
            }
            else if (length(...j.end, ...i.position) < j.radius + i.radius) {
                i.position = j.start.map((val, index) => val + j.end[index] - i.position[index]);
            }
        }
    }
};
const mainLoop = (delta) => {
    const time = delta * parseFloat(memory.warp.toString());
    applyGravity(time);
    move(time);
    checkTeleportation();
};
const start = (data) => commands["start"]("");
loop.setUpdate(mainLoop).setDraw(draw);
loadSystem(initialSystem);
export { run, back, addRedPortal, addBluePortal, start };
