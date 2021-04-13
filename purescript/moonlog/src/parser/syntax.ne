@{%
const { lexer, convertToken, convertTokenId } = require("./lexer.ts")
const { var_, natural, constructor, list, pattern, rule } = require("./ast.ts")
%}

@lexer lexer

expression -> ___:? toplevel {% p => p[1] %}

toplevel -> rule:* {% id %}

rule 
    -> pattern nl ___:? %indent ___ ruleBody (___:?) {% ([pattern,,,,,body]) => rule(pattern, body) %}
    | pattern (___:?) {% ([head]) => rule(head, []) %}

ruleBody
    -> pattern (___:?) %dedent {% ([p]) => [p] %}
    | pattern ___ ruleBody {% ([current,,next]) => [current, ...next] %} 

pattern -> patternName _ (term _ {% id %}):* {% ([name, _, arguments_]) => constructor(name, arguments_) %}

term 
    -> natural {% ([a]) => natural(a) %} 
    | identifier {% ([a]) => var_(a) %} 
    | patternName {% ([a]) => pattern(constructor(a, [])) %}
    | "(" _ termInParens _ ")" {% i => i[2] %}

termInParens
    -> pattern {% ([c]) => pattern(c) %} 
    | term {% id %}

# Lexing
identifier -> %identifier {% convertTokenId %}
patternName -> %constructor {% convertTokenId %}
natural -> %natural {% convertTokenId %}

___ -> (nl | __):+
nl -> %newline
__ -> %whitespace
_ -> %whitespace:?
