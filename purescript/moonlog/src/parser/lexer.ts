import moo from "moo";
// @ts-ignore
import IndentationLexer from "moo-indentation-lexer";
import type { SourcePosition, Token } from "./ast";

// ========== Actual lexer stuff
const mooLexer = moo.compile({
  whitespace: /[ \t]+/,
  newline: { match: /(?:\r\n?|\n)+/, lineBreaks: true },
  punctuation: [",", "(", ")", "[", "]", "|"],
  natural: /[0-9]+/,
  identifier: { match: /[a-z_]['\-_a-zA-Z0-9]*/ },
  constructor: { match: /[A-Z]['\-_a-zA-Z0-9]*/ },
});

export const lexer: moo.Lexer = new IndentationLexer({
  lexer: mooLexer,
  newlineType: "newline",
  indentationType: "whitespace",
});

// ========== Source position related stuff
export const tokenStart = (token: moo.Token): SourcePosition => ({
  line: token.line,
  column: token.col - 1,
});

export function tokenEnd(token: moo.Token): SourcePosition {
  const lastNewLine = token.text.lastIndexOf("\n");

  if (lastNewLine !== -1) {
    throw new Error("Unsupported case: token with line breaks");
  }

  return {
    line: token.line,
    column: token.col + token.text.length - 1,
  };
}

export const convertToken = (token: moo.Token): Token => ({
  type: token.type as Token["type"],
  text: token.value,
  span: {
    start: tokenStart(token),
    end: tokenEnd(token),
  },
});

export const convertTokenId = (data: moo.Token[]): Token =>
  convertToken(data[0]);
