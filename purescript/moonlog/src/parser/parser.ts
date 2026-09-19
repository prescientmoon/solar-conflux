// @ts-ignore nearley import
import syntax from "./syntax.ne";
import { Grammar, Parser } from "nearley";

const mkParser = (start: string) => {
  const grammar = Grammar.fromCompiled({ ...syntax, ParserStart: start });

  return (input: string) => {
    const parser = new Parser(grammar);

    parser.feed(input);

    return parser.results[0];
  };
};

export const parse = mkParser("expression");
export const parseConstructor = mkParser("pattern");
