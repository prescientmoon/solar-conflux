const canvas = document.getElementById("canvas");
const ctx = canvas.getContext("2d");

canvas.height = window.innerHeight;
canvas.width = window.innerWidth;
const w = window.innerWidth;
const h = window.innerHeight;

const points = [];
const pointSize = 5;
const shuffleProcent = 0.99;

let pressing = false;
let a = 1;
let b = 0;
let rate = 0.5;

const sumArrays = (a,c) => [a[0] + c[0],a[1] + c[1]];

MainLoop.setDraw(() => {
    ctx.fillStyle = "#000000";
    ctx.fillRect(0,0,3000,3000);
    ctx.fillStyle = "#ffffff";
    for (let i of points){
        ctx.fillRect(i[0] * w,i[1] * h,pointSize,pointSize);
    }
    ctx.strokeStyle = "#8888ff";
    ctx.beginPath();
    ctx.moveTo(0,b * h);
    ctx.lineTo(w,h * a + h * b);
    ctx.stroke();
    if (points.length > 1){
        let good = regression();
        ctx.strokeStyle = "#22ff55";
        ctx.beginPath();
        ctx.moveTo(0,h * good[1]);
        ctx.lineTo(w,h * good[0] + h * good[1]);
        ctx.stroke();
    }
}).setUpdate(train).start();

$(canvas).mousedown((e) => {pressing = true});
$(canvas).mouseup((e) => {pressing = false});

$(canvas).mousemove(e => {
    if (pressing){
        points.push([
            e.clientX/w,
            e.clientY/h
        ]);
    }
});

function shuffle(a) {
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}

function train(time){
    if (Math.random() > shuffleProcent) points = shuffle(points);
    let lastcost = Math.pow(10,20);
    if (points.length > 1){
        for (let i of points){
            const x = i[0];
            const y = i[1];

            let guess = a * x + b;
            let error = y - guess;

            a = a + error * rate * x;
            b = b + error * rate;

            guess = a * x + b;
            newerror = y - guess;
        }
    }
}

function regression(){
    const sum = points.reduce(sumArrays);
    const mean = sum.map(val => val/points.length);

    let den = 0;
    let num = 0;

    for (let i of points){
        num += (i[0] - mean[0]) * (i[1] - mean[1]);
        den += (i[0] - mean[0]) * (i[0] - mean[0]);
    }

    let m = num/den;
    let n = mean[1] - m * mean[0];

    return [m,n];
}
