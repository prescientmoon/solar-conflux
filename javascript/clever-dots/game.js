function game() {
  this.size = [500, 500];

  this.clear = function () {
    var c = document.getElementById("can");
    var ctx = c.getContext("2d");
    ctx.fillStyle = "#000000";
    ctx.fillRect(0, 0, 1000, 1000);
    //console.log("clearing");
  };

  this.draw = function (x, y) {
    var c = document.getElementById("can");
    var ctx = c.getContext("2d");
    ctx.fillStyle = "#FFFFFF";
    ctx.fillRect(x, y, 1, 1);
    //console.log(y+"drawing"+x);
    //console.log("finished drawing");
  };
}
var a = new game();
a.clear();
var b = new population(a);
b.reset();
b.create_population();

function redraw() {
  let done = false;
  let callback = null;
  requestAnimationFrame(() => {
    done = true;
    if (callback !== null) callback();
  });

  return new Promise((res) => {
    if (done) res();
    else callback = () => res();
  });
}

const iterationsPerFrame = 10;

async function main() {
  for (let k = 0; k < 1000; k++) {
    await redraw();
    a.clear();
    console.log(`thinking: ${k}`);
    for (let i = 0; i < 500; i++) {
      if (i % iterationsPerFrame === 0) await redraw();
      for (let j = 0; j < b.Population.length; j++) {
        b.think(b.Population[j]);
        a.draw(b.Population[j].x, b.Population[j].y);
      }
    }

    b.evolve();
  }
}

main();

