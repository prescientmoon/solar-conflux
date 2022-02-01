@{%
const { lexer, convertToken, convertTokenId } = require("./lexer.ts")
const { namedHole, star, var_ } = require("./ast.ts")
%}

@lexer lexer

assume -> "assume" ___ "("
var -> %identifier% {% ([token]) => var_(convertToken(token)) %}
namedHole -> %namedHole% {% ([token]) => namedHole(convertToken(token)) %}
star -> "*" {% ([token]) => star(convertToken(token)) %}

# Lexing
identifier -> %identifier {% convertTokenId %}
natural -> %natural {% convertTokenId %}

___ -> (nl | :whitespace __):+
nl -> %newline
__ -> %whitespace
_ -> %whitespace:?