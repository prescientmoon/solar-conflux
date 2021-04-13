import { parse } from "./parser/parser";

const util = require("util");

const log = (o) =>
  console.log(util.inspect(o, { showHidden: false, depth: null }));

log(
  parse(`
Add (S n) b c
    Add n (S b) c
Add 0 b b

Sub a 0 a
Sub (S a) (S b) c
    Sub a b c`)
);
