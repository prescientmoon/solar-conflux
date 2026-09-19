import {
  withSpan,
  WithSpan,
  Term,
  Token,
  Declarations,
  Declaration,
  printAst,
} from "./ast";
import { convertToken, mooLexer } from "./lexer";

export class MyLexer {
  public readonly tokens: moo.Token[];
  public at: number = 0;
  public minIndendation = 0;

  public constructor(lexer: moo.Lexer, input: string) {
    lexer.reset(input);

    this.tokens = [...lexer];
  }

  private skipWs(checkIndent: boolean = true): number | null {
    let indent = null;

    while (true) {
      const next = this.tokens[this.at];

      if (next === undefined) return null;

      if (next.type === "whitespace" || next.type === "newline") {
        if (next.type === "newline") {
          indent = 0;
        } else if (indent !== null) indent += next.text.length;

        this.at++;
      } else {
        if (checkIndent && indent !== null && indent < this.minIndendation)
          throw new Error(
            `Token ${
              this.tokens[this.at].text
            } has indentation ${indent}, which is smaller than the minimum required indentation of ${
              this.minIndendation
            }`
          );

        return indent;
      }
    }
  }

  public withMinIndentation<A>(amount: number, go: () => A): A {
    const before = this.minIndendation;

    this.minIndendation = amount;
    const result = go();
    this.minIndendation = before;

    return result;
  }

  public consumeIndent(amount: number) {
    const indent = this.skipWs(false);

    if (amount === 0 && indent === null) return;

    if (indent === null)
      throw new Error(
        `Expected indentation, found ${this.peek().text} instead`
      );

    if (indent !== amount)
      throw new Error(
        `Expected an indentation of ${amount}, found ${this.peek()} instead`
      );
  }

  public peek(): moo.Token {
    this.skipWs();
    return this.tokens[this.at];
  }

  public next(): moo.Token {
    this.skipWs();
    return this.tokens[this.at++];
  }

  public savePosition<A>(go: () => A): A {
    const state = this.at;
    const result = go();
    this.at = state;
    return result;
  }
}

export class Parser {
  public constructor(public lexer: MyLexer) {}

  private next(): Token {
    const next = this.lexer.next();

    if (next === undefined) throw new Error("Unexpected eof");

    return convertToken(next);
  }

  private peekNoEof(): Token {
    const token = this.peek();

    if (token === null) throw new Error(`No tokens left:(`);

    return token;
  }

  private peek() {
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

  private migthBacktrack<A>(parse: (commit: () => void) => A): A | null {
    let commited = false;

    const state = this.lexer.at;
    try {
      return parse(() => {
        commited = true;
      });
    } catch (e) {
      if (commited) throw e;
      else {
        this.lexer.at = state;
        return null;
      }
    }
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

  private many<A>(inside: (commit: () => void) => A): A[] {
    const result = [];

    while (true) {
      const parsed = this.migthBacktrack(inside);

      if (parsed === null) {
        return result;
      }

      result.push(parsed);
    }
  }

  private some<A>(inside: (commit: () => void) => A): A[] {
    const first = inside(() => {});

    const rest = this.many(inside);

    return [first, ...rest];
  }

  private parseIdentifier() {
    const next = this.next();

    if (next.type === "identifier") return next;
    if (next.text === "_") return next;

    throw new Error(`Unexpected ${next.text}. Expected identifier`);
  }

  // ========== Actual parsing
  private parseAtom(commit: () => void = () => {}): WithSpan<Term> {
    const token = this.peekNoEof();

    if (token.text === "*") {
      commit();
      this.next();
      return withSpan<Term>(
        {
          type: "star",
          value: {},
        },
        token.span
      );
    } else if (token.text === "(") {
      commit();
      const result = this.migthBacktrack((commit) => {
        this.next();
        const args = this.separated<Token>(",", () => this.parseIdentifier());

        if (args.length > 1) commit();

        this.expect(":");
        commit();

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
      });

      if (result === null) {
        return this.surrounded("(", ")", () => this.parseTerm());
      }

      return result;
    } else if (token.text === "_") {
      commit();
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
      commit();
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
      commit();
      throw new Error("Don't know how to handle numbers yet"); // By don't know, I mean I haven't implemented them yet
    } else if (token.text === "\\") {
      commit();
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
      commit();
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
      commit();
      this.next();
      const declarations = this.parseDeclarations(false);
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
      commit();
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

  public parseTerm(commit: () => void = () => {}): WithSpan<Term> {
    let atom = this.parseAtom(commit);

    const args = this.many<WithSpan<Term>>((commit) => this.parseAtom(commit));

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

    const bigger = this.migthBacktrack((commit) => {
      const token = this.peekNoEof();

      if (token.text === "->") {
        commit();
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
        commit();
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
    });

    return bigger || atom;
  }

  public parseDeclarations(toplevel = false): Declarations {
    let indentation = 0;

    if (!toplevel) {
      this.lexer.savePosition(() => {
        const indentationHint = this.peekNoEof();

        indentation = indentationHint.span.start.column;
      });
    }

    return this.lexer.withMinIndentation<Declarations>(indentation + 1, () => {
      return this.some<Declaration>((commit) => {
        this.lexer.consumeIndent(indentation);
        const name = this.parseIdentifier();

        commit();
        const after = this.peekNoEof();

        let annotation: WithSpan<Term> | null = null;

        if (after.text === "::") {
          this.next();
          annotation = this.parseTerm();

          this.lexer.consumeIndent(indentation);

          const nameAgain = this.parseIdentifier();
          if (nameAgain.text !== name.text)
            throw new Error(
              `Cannot find implementation for annotation ${name.text}. Found ${nameAgain.text} instead`
            );
        }

        const args = this.many(() => this.parseIdentifier());

        this.expect("=");

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
            {
              start: token.span.start,
              end: result.span.end,
            }
          );
        }

        if (annotation !== null)
          result = withSpan<Term>(
            {
              type: "annotation",
              value: {
                type: annotation,
                value: result,
              },
            },
            {
              start: name.span.start,
              end: result.span.end,
            }
          );

        return {
          name: withSpan(name.text, name.span),
          value: result,
        } as Declaration;
      });
    });
  }
}
