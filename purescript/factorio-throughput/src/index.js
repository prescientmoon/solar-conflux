import { main } from "Main.purs";
import functionPlot from "function-plot";

let lastId = 0;
const root = document.body;

const width = 400;
const height = 250;

const render = (name) => (functions) => () => {
  const currentId = ++lastId;

  console.log("Renering!!!");

  const node = document.createElement("div");
  node.id = currentId;
  node.title = name;
  node.className = "graph";

  root.appendChild(node);

  const functionData = functions.map((fn) => ({
    fn: (scope) => fn(scope.x),
    graphType: "polyline",
  }));

  functionPlot({
    target: node,
    width,
    height,
    yAxis: { domain: [-5, 35] },
    xAxis: { domain: [-1, 5] },
    grid: true,
    data: functionData,
  });
};

main(render)();
