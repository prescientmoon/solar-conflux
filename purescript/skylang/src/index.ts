import { resolve } from "path";
import { readFileSync } from "fs";
import { MyLexer, Parser } from "./Parser/parser";
import { mooLexer } from "./Parser/lexer";
import { printAst } from "./Parser/ast";
// @ts-ignore
import { main } from "../output/Sky.Main";

const file = resolve(__dirname, "../examples/basic.sky");

const content = readFileSync(file, { encoding: "utf-8" });

const parser = new Parser(new MyLexer(mooLexer, content));
const ast = parser.parseDeclarations(true);

main(ast)();

try {
} catch (e) {
  throw mooLexer.formatError(
    parser.lexer.tokens[parser.lexer.at],
    (e as any).toString()
  );
}
