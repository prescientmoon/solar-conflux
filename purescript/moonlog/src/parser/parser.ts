// @ts-ignore nearley import
import syntax from "./syntax.ne";
import { Grammar, Parser } from "nearley";

const grammar = Grammar.fromCompiled(syntax);

export const parse = (input: string) => {
  const parser = new Parser(grammar);

  parser.feed(input);

  return parser.results[0];
};
