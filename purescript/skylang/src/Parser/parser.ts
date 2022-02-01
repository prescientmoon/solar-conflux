import { Lexer } from "moo";
import {
  withSpan,
  WithSpan,
  Term,
  Token,
  Declaration,
  Declarations,
} from "./ast";
import { convertToken, mooLexer } from "./lexer";

class MyLexer {
  private tokens: moo.Token[];
  public at: number = 0;

  public constructor(lexer: moo.Lexer, input: string) {
    lexer.reset(input);

    this.tokens = [...lexer];
  }

  private skipWs() {
    while (true) {
      const next = this.tokens[this.at];

      if (next === undefined) return;

      if (next.type === "whitespace" || next.type === "newline") this.at++;
      else return;
    }
  }

  public peek(): moo.Token {
    this.skipWs();
    return this.tokens[this.at];
  }

  public next(): moo.Token {
    this.skipWs();
    return this.tokens[this.at++];
  }
}

export class Parser {
  public constructor(public lexer: MyLexer) {}

  private next(): Token {
    const next = this.lexer.next();

    if (next === undefined) throw new Error("Unexpected eof");

    return convertToken(next);
  }

  private peek(): Token | null {
    const token = this.lexer.peek();

    if (token === undefined) return null;

    return convertToken(token);
  }

  private expect(text: string) {
    const next = this.next();

    if (next.text === text) return next;

    throw new Error(`Expected ${text}, but found ${next.text} instead`);
  }

  private surrounded<A>(start: string, end: string, inside: () => A): A {
    this.expect(start);
    const result = inside();
    this.expect(end);

    return result;
  }

  private separated<A>(by: string, inside: () => A): A[] {
    const result = [];

    while (true) {
      result.push(inside());

      const next = this.peek();

      if (next === null || next.text !== by) return result;

      this.next();
    }
  }

  private many<A>(inside: () => A): A[] {
    const result = [];

    while (true) {
      const state = this.lexer.at;
      try {
        const parsed = inside();
        result.push(parsed);
      } catch {
        this.lexer.at = state;
        return result;
      }
    }
  }

  private some<A>(inside: () => A): A[] {
    const first = inside();

    const rest = this.many(inside);

    return [first, ...rest];
  }

  private parseIdentifier() {
    const next = this.next();

    if (next.type === "identifier") return next;

    throw new Error(`Unexpected ${next.text}. Expected identifier`);
  }

  private attempt<A>(to: () => A): A | null {
    try {
      return to();
    } catch {
      return null;
    }
  }

  // ========== Actual parsing
  private parseAtom(): WithSpan<Term> {
    const token = this.peek();

    if (token === null) this.next(); // Throw eof error

    if (token.text === "*") {
      this.next();
      return withSpan<Term>(
        {
          type: "star",
          value: {},
        },
        token.span
      );
    } else if (token.text === "(") {
      const state = this.lexer.at;
      let shouldRecover = true;
      try {
        this.next();
        const args = this.separated<Token>(",", () => this.parseIdentifier());

        this.expect(":");
        shouldRecover = false;
        const domain = this.parseTerm();
        this.expect(")");
        this.expect("->");
        let result = this.parseTerm();
        const overallSpan = {
          start: token.span.start,
          end: result.span.end,
        };

        for (let i = args.length - 1; i >= 0; i--) {
          const token = args[i];

          result = withSpan<Term>(
            {
              type: "pi",
              value: {
                name: withSpan(token.text, token.span),
                codomain: result,
                domain,
              },
            },
            overallSpan
          );
        }

        return result;
      } catch (err) {
        if (!shouldRecover) throw err;

        this.lexer.at = state;
        return this.surrounded("(", ")", () => this.parseTerm());
      }
    } else if (token.text === "_") {
      this.next();

      return withSpan<Term>(
        {
          type: "hole",
          value: {
            name: null,
          },
        },
        token.span
      );
    } else if (token.type === "namedHole") {
      this.next();

      return withSpan<Term>(
        {
          type: "hole",
          value: {
            name: token.text.slice(1),
          },
        },
        token.span
      );
    } else if (token.type === "natural") {
      throw new Error("Don't know how to handle numbers yet"); // By don't know, I mean I haven't implemented them yet
    } else if (token.text === "\\") {
      this.next();
      const args = this.some(() => this.parseIdentifier());

      this.expect("->");

      let result = this.parseTerm();

      for (let i = args.length - 1; i >= 0; i--) {
        const token = args[i];

        result = withSpan<Term>(
          {
            type: "lambda",
            value: {
              argument: withSpan(token.text, token.span),
              body: result,
            },
          },
          token.span
        );
      }

      return result;
    } else if (token.text === "assume") {
      this.next();
      const inner = this.parseAtom();

      return withSpan<Term>(
        {
          type: "assumption",
          value: {
            type: inner,
          },
        },
        token.span
      );
    } else if (token.text === "let") {
      this.next();
      const declarations = this.parseDeclarations();
      this.expect("in");

      let result = this.parseTerm();

      const bigSpan = {
        start: token.span.start,
        end: result.span.end,
      };

      for (let i = declarations.length - 1; i >= 0; i--) {
        const definition = declarations[i];

        result = withSpan<Term>(
          {
            type: "let",
            value: {
              body: result,
              definition,
            },
          },
          bigSpan
        );
      }

      return result;
    } else if (token.type === "identifier") {
      this.next();

      return withSpan<Term>(
        {
          type: "var",
          value: {
            name: token.text,
          },
        },
        token.span
      );
    }

    throw new Error(`Unexpected token ${token.text}`);
  }

  public parseTerm(): WithSpan<Term> {
    let atom = this.parseAtom();
    const args = this.many<WithSpan<Term>>(() => this.parseAtom());

    for (const arg of args) {
      atom = withSpan<Term>(
        {
          type: "application",
          value: {
            function: atom,
            argument: arg,
          },
        },
        {
          start: atom.span.start,
          end: arg.span.end,
        }
      );
    }

    const token = this.peek();

    if (token === null) return atom;

    if (token.text === "->") {
      this.next();

      const codomain = this.parseTerm();

      return withSpan<Term>(
        {
          type: "pi",
          value: {
            domain: atom,
            codomain,
            name: null,
          },
        },
        {
          start: atom.span.start,
          end: codomain.span.end,
        }
      );
    }

    if (token.text === "::") {
      this.next();

      const annotation = this.parseTerm();

      return withSpan<Term>(
        {
          type: "annotation",
          value: {
            type: annotation,
            value: atom,
          },
        },
        {
          start: atom.span.start,
          end: annotation.span.end,
        }
      );
    }

    return atom;
  }

  private parseDeclarations(): Declarations {
    throw new Error("Not implemented");
  }
}

export function parseExpression(input: string): WithSpan<Term> {
  const parser = new Parser(new MyLexer(mooLexer, input));

  return parser.parseTerm();
}
