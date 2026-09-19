// ========== Lexing stuff
export type SourcePosition = { line: number; column: number; index: number };
export type SourceSpan = { start: SourcePosition; end: SourcePosition };
export type TokenType =
  | "whitespace"
  | "newline"
  | "keyword"
  | "punctuation"
  | "namedHole"
  | "natural"
  | "identifier";
export type Token = { type: TokenType; text: string; span: SourceSpan };

export type WithSpan<T> = { value: T; span: SourceSpan };

// ========== Types
/** Js representation for variants */
export type Adt<T extends object> = {
  [key in keyof T]: { type: key; value: T[key] };
}[keyof T];

export type Declarations = Array<Declaration>;
export type Declaration = {
  name: WithSpan<string>;
  value: WithSpan<Term>;
};

export type Term = Adt<{
  star: {};
  hole: {
    name: string | null;
  };
  var: {
    name: string;
  };
  lambda: {
    argument: WithSpan<string>;
    body: WithSpan<Term>;
  };
  application: {
    function: WithSpan<Term>;
    argument: WithSpan<Term>;
  };
  annotation: {
    value: WithSpan<Term>;
    type: WithSpan<Term>;
  };
  pi: {
    name: WithSpan<string> | null;
    domain: WithSpan<Term>;
    codomain: WithSpan<Term>;
  };
  assumption: {
    type: WithSpan<Term>;
  };
  let: {
    definition: Declaration;
    body: WithSpan<Term>;
  };
}>;

// ========== Constructors
export function withSpan<A>(value: A, span: SourceSpan): WithSpan<A> {
  return {
    span,
    value,
  };
}

export function namedHole(token: Token): WithSpan<Term> {
  const actualName = token.text.slice(1);

  return withSpan<Term>(
    {
      type: "hole",
      value: {
        name: actualName.length ? actualName : null,
      },
    },
    token.span
  );
}

export function star(token: Token): WithSpan<Term> {
  return withSpan<Term>(
    {
      type: "star",
      value: {},
    },
    token.span
  );
}

export function var_(token: Token): WithSpan<Term> {
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

// ========== Debug helpers
export function printAst(term: Term): string {
  if (term.type === "star") return "*";
  else if (term.type === "hole") return "?" + (term.value.name || "");
  else if (term.type === "var") return term.value.name;
  else if (term.type === "lambda")
    return (
      "\\" +
      term.value.argument.value +
      " -> " +
      printAst(term.value.body.value)
    );
  else if (term.type === "assumption")
    return "assume " + printAst(term.value.type.value);
  else if (term.type === "pi")
    return (
      "(" +
      (term.value.name?.value || "_") +
      ": " +
      printAst(term.value.domain.value) +
      ") -> " +
      printAst(term.value.codomain.value)
    );
  else if (term.type === "application")
    return `(${printAst(term.value.function.value)}) (${printAst(
      term.value.argument.value
    )})`;
  else if (term.type === "annotation")
    return `(${printAst(term.value.value.value)}) :: (${printAst(
      term.value.type.value
    )})`;
  else if (term.type === "let")
    return `let ${term.value.definition.name.value} = ${printAst(
      term.value.definition.value.value
    )} in ${printAst(term.value.body.value)}`;

  throw new Error("Not implemented");
}
