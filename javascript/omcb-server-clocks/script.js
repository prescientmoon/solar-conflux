const fs = require("node:fs");

const data = fs.readFileSync("./data.txt", "utf8");

const numbers = [];
for (const line of data.split("\n")) {
  if (line.trim() === "") continue;
  const numberLine = [];
  numbers.push(numberLine);

  for (const segment of line.split(" ")) {
    if (segment.startsWith("?")) continue;
    const isMid = segment.endsWith(".");
    const num = parseInt(segment);
    // if (isMid) numberLine.push((num - 1) * 2 + 1);
    // else numberLine.push((num - 1) * 2);
    if (isMid) numberLine.push(12 + num - 1);
    else numberLine.push(num - 1);
  }
}

const alphabet = "abcdefghijklmnopqrstuvwxyz";

for (let shift = 0; shift < alphabet.length; shift++) {
  for (const line of numbers) {
    let res = "";
    for (const num of line) {
      res += alphabet[(num + shift) % alphabet.length];
    }
    for (let i = 0; i < res.length; i++)
      if (res.substr(i).startsWith("doit")) console.log(res);
  }
  console.log(" ");
}
