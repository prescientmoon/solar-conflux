import { resolve } from "path";
import { readFileSync } from "fs";
import { MyLexer, Parser } from "./Parser/parser";
import { mooLexer } from "./Parser/lexer";
import { printAst } from "./Parser/ast";

const file = resolve(__dirname, "../examples/basic.sky");

const content = readFileSync(file, { encoding: "utf-8" });

const parser = new Parser(new MyLexer(mooLexer, content));

console.log(
  parser.parseDeclarations(true).map((a) => ({
    name: a.name.value,
    value: printAst(a.value.value),
  }))
);
try {
} catch (e) {
  throw mooLexer.formatError(
    parser.lexer.tokens[parser.lexer.at],
    (e as any).toString()
  );
}
