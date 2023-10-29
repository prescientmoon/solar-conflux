#set heading(numbering: "1.", supplement: "Chapter")

#let numbered(body) = body
#show numbered: set math.equation(numbering: "(1)")

#set text( size: 14pt)

#set page(
  paper: "a5",
  margin: (x: 1.8cm, y: 1.5cm),
)

#set par(
  justify: true,
  leading: 0.52em,
)

= Hi

How are you? I am typing this in typst!

== Subsection!

- List item
- Other kind of lists:
  + This is the other kind of lists!
  + Again :O

This vim plugin works surprisingly well!

This is a math formula:
#numbered[ a + b = rho] 
#numbered[
  $ lambda &= alpha + beta \
    lambda ' &= beta rho \ 
  $
] <test>
$ a + b = rho $
#math.equation[ a + b = rho ]

Check this out --- @test.

I can also write math inline like here $lim_(x -> 100) x^2 = 10^4$.

I can do --- and --, pretty neat!

#par[#lorem(10)]

=== Subsubsection <labelled>

See @labelled for more details!
