# Steiner

Ml style language I'm making for fun

## Implemented stuff:

- String/Float literals
- If & let expressions
- Type annotations
- Lambdas
- Syntactic sugar for declaring lambdas (`id = \x -> x` can be written as `id x = x`)
- Function application
- Type inference

## Stuff to do

- Operators (I already have the lexer parse those but I'd need a more complex parser for this)
- Understandable errors
- Actual cli
- Commands (eg `:type`)
- Typed holes
- more type system stuff (pretty basic atm)
- top level syntax
- modules
- ADTS
- Make parser error tolerant
- Allow explicit forall syntax
- A lot more stuff which I didn't think of in the minute it took me to make this readme
