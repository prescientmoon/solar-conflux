// Get Polylux from the official package repository
#import "@preview/polylux:0.4.0": *

#set page(paper: "presentation-16-9")
#set text(size: 20pt, font: "Cascadia Code")

#enable-handout-mode(json(bytes(sys.inputs.at("HANDOUT", default:"false"))))

#slide[
  #set page(fill:rgb(245,169,184))
  #set text(fill: white, size:35pt)
  #set align(horizon+center)
  = (｡ι‿ι｡) Church-encoding text into iotas
]

#let clam = [#text(fill: rgb(60,150,180), $#sym.lambda$)]
#let cdot = [#text(fill: rgb(60,150,180), $.$)]
#let la(a, b) = [#clam #a #cdot #{h(0.5em)} #b]
#let S = text(fill: rgb(255, 41, 155), $#math.op("S")$)
#let K = text(fill:green,$#math.op("K")$)
#let I = text(fill:rgb(145,145,145),$#math.op("I")$)
#let zero = text(fill:rgb(145,145,145),$#math.op("zero")$)
#let succ = text(fill:purple,$#math.op("succ")$)
#let nil = text(fill:red,$#math.op("nil")$)
#let cons = text(fill:orange,$#math.op("cons")$)
#let iota = text(fill: purple, $#sym.iota$)
#let nats = text(fill: red, $ℕ$)
#let char(c) = text(fill:green, raw("'" + c + "'"))


#slide[
  #set text(size: 25pt)
  = The backstory
  #v(1em)
  #set text(size: 18pt)

  // They say that a peculiar QR code was lying on the final slide of our bachelor prep presentation. A promise of a prize, awaiting those about to scan the code. None lingered long enough to pierce the iota-veiled puzzle, but worry not, for the meaning of the runes shall soon reveal itself to those who seek it.

  #align(center+horizon)[
  #grid(columns:(45%, 45%),
    [
      #image("images/final-slide-fbp.png", height: 50%)
      A mysterious QR code...
    ],
    align(bottom+right)[
      #show: later
      #v(3em)
      ...leading to peculiar runes
      #image("images/iota-manuscript.png", height: 50%)
    ]
  )
  ]
]

#slide[
  #set align(horizon)
  = The $clam$-calculus
  #v(1em)

  1. Lamdas: instead of $x ↦ ...$ we write $la(x,...)$

  #show: later
  #v(0.15em)

  2. Variable references: for example $la(x, x)$ denotes the identity.

  #show: later
  #v(0.15em)

  3. Function applications: instead of $f(x)$ we write $f x$.
]


#slide[
  #set text(size: 25pt)
  = Multi-parameter functions
  #v(1em)
  #set text(size: 23pt)

  Problem: no products
  $
  f &: nats #sym.times nats -> nats \
  f &: (a, b) ↦ a + b
  $

  #show: later
  Solution: currying!
  $
  g &: nats -> nats -> nats \
  g &: a ↦ (b ↦ a + b)
  $

  #align(bottom)[
  #set text(size: 11pt)
  Note: the two are equivalent in a sense because something something adjoint functors, don't worry about it.
  ]
]

#slide[
  #set text(size: 25pt)
  = The #S#K#I combinators
  #v(1em)
  #set text(size: 35pt)

  #align(horizon)[$
  #I &:= la(x,x) \
  #K &:= la(a,la(b, a)) \
  #S &:= la(f, la(a, la(c, (f c) (a c))))
  $]
]

#slide[
  #set text(size: 25pt)
  #grid(columns:(auto, auto), gutter:20pt, [
  = The #S#K#I combinators
  #v(1em)
  #set text(size: 23pt)
  #set align(horizon)

  Any term in the $clam$-calculus can be written using nothing but the #S#K combinators.

    #show: later
    For example $#I := #S #K #S$.
    ],[
  #set align(horizon+center)
    #uncover(2)[
    #image("images/ithinkshespartoftheskteam.jpg", height:80%)
    ]
  ])
]

#slide[
  #set text(size: 25pt)
  = A combinator to conquer them all
  #v(1em)
  #grid(columns:(auto, auto), gutter: 5pt,[
  #set text(size: 18pt)

  Enter _the $iota$-combinator_!
  $ iota := la(x, x #S #K) = #S (#S #I (#K #S))(#K #K). $

  #show:later

  We can then define the #S#K#I combinators in terms of $iota$:
  $
  #I &= iota iota \
  #K &= iota( iota (iota iota)) \
  #S &= iota(iota(iota(iota iota)))
  $
  ],[
    #align(horizon)[
      #uncover(2)[
    #image("images/alwayshasbeenallfunctions.jpg")
      ]
    ]
  ])
]

#slide[
  #set text(size: 25pt)
  = Defining $nats$
  #v(1em)
  #set text(size: 20pt)

  #set align(horizon)
  - Problem: we normally define $nats$ using $zero$ and $succ$, but we have neither of those things.
  - Solution: ask the person using the numbers to provide them!
]

#slide[
  #set text(size: 25pt)
  = Defining $nats$
  #v(1em)
  #set text(size: 20pt)

  Example: instead of defining $1 := succ(zero)$, let $ 1 := la(zero, la(succ, succ " " zero)). $

  #show:later
  We can keep going!
  $
  2 &:= la(zero, la(succ, succ (succ " " zero)) ) \
  3 &:= la(zero, la(succ,succ( succ (succ " " zero)))) \
  4 &:= la(zero, la(succ,succ(succ( succ (succ " " zero)))))\
  dots.v " " &
  $

]

#slide[
  #set text(size: 25pt)
  = Working with $nats$
  #v(1em)
  #set text(size: 18pt)

  How do we define addition?
  1. The function takes two arguments: $la(a, la(b, ...))$
  #show:later
  2. The result is also a natural: $la(a, la(b, la(zero, la(succ, ...))))$
  // #show:later
  // 3. The result likely involves $a$: $la(a, la(b, la(zero, la(succ, a " " ? " " ?))))$
  #show:later
  3. Idea — return $a$, but with its concept of "$zero$" defined as the natural given by $b$: $la(a, la(b, la(zero, la(succ, a " " (b " " zero " " succ) " " succ))))$

 $ a + b := la(zero, la(succ, a " " (b " " zero " " succ) " " succ)). $
]

#slide[
  #set text(size: 30pt)
  = Ordered lists
  #v(1em)
  #set text(size: 20pt)

  #align(horizon)[
  Ordered lists are surprisingly similar to $nats$. We need:
    - The empty list (call it $nil$)
    - A way to go from $a_1, a_2, ..., a_n$ to $x, a_1, a_2, ..., a_n$ (call it $cons$)

    #show:later
    Example: We can write $[1, 2, 3]$ as $ la(nil, la(cons, cons " " 1 " " (cons " " 2 " " (cons " " 3 " " nil)))) $
  ]

  #align(bottom)[
  #set text(size: 11pt)
  #uncover(2)[
  Note: the naturals are essentially just ordered lists of the unit type, but don't worry about that.
    ]
  ]
]

#slide[
  #grid(columns:(auto, auto),
    gutter: 10pt,
  [
#set text(size: 23pt)
#v(1em)
= The general principle
#v(1em)
#set text(size: 25pt)

    #align(horizon)[
    - We can encode any inductively defined structure into the $clam$-calculus this way
    - Named after the logician *Alonzo Church*
    ]
  ],
  image("images/alonzo-church.jpg", height:100%)
  )
]

#slide[
  #set text(size: 30pt)
  = Encoding text as iotas
  #v(1em)
  #set text(size: 25pt)
  #align(horizon)[
    - Strings of text are nothing but ordered lists of characters.
    - Characters can be represented as naturals (indices inside some alphabet)
  ]
]

#slide[
  #set text(size: 30pt)
  = Encoding text as iotas
  #v(1em)
  #set text(size: 25pt)
  #align(horizon)[
    1. Take the input string, and church-encode it:
    $ #text(fill:green,```"meow"```) -> #image("images/un-executed-church-encoding.png",height:3em) $
    #show:later
    2. Normalize (evaluate) the output:
  #set text(size: 15pt)
    ```λλ1(λλ1(1(1(1(1(1(1(1(1(1(1(1 0))))))))))))(1(λλ1(1(1(1 0))))(1(λλ1(1(1(1(1(1(1(1(1(1(1(1(1(1 0))))))))))))))(1(λλ1(1(1(1(1(1(1(1(1(1(1(1(1(1(1(1(1(1(1(1(1(1 0)))))))))))))))))))))) 0)))```

  // #set text(size: 10pt)
// Without expanding the naturals, the above is essentially
    // $ la(nil,la(cons,cons " " #char("m") " " (cons " " #char("e") " " (cons " " #char("o") " " (cons " " #char("w") " " nil))))) $
  ]
]
#slide[
  #set text(size: 30pt)
  = Encoding text as iotas
  #v(1em)
  #set text(size: 25pt)
  #align(horizon)[
    3. Convert to #S#K combinators
  #set text(size: 15pt)
    ```S(S(KS)(S(KK)(SI(K(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(KI)))))))))))))))))(S(S(KS)(S(KK)(SI(K(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(KI)))))))))(S(S(KS)(S(KK)(SI(K(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(KI)))))))))))))))))))(S(S(KS)(S(KK)(SI(K(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(S(S(KS)(S(KK)I))(KI)))))))))))))))))))))))))))(KI))))```
  ]
]

#slide[
  #set text(size: 20pt)
  = The final result
  #set text(size: 9pt)
  #align(horizon)[
```ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ι(ι(ι(ιι)))(ιι)(ι(ι(ιι))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ιι))(ιι))))))))))))))))))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ι(ι(ι(ιι)))(ιι)(ι(ι(ιι))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ιι))(ιι))))))))))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ι(ι(ι(ιι)))(ιι)(ι(ι(ιι))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ιι))(ιι))))))))))))))))))))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ι(ι(ι(ιι)))(ιι)(ι(ι(ιι))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ι(ιι)))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ι(ιι)))))(ι(ι(ι(ιι)))(ι(ι(ιι))(ι(ι(ιι))))(ιι)))(ι(ι(ιι))(ιι))))))))))))))))))))))))))))(ι(ι(ιι))(ιι)))))```
  ]
]

#slide[
  #set page(fill:rgb(245,169,184))
  #set text(fill: white)
  #align(center+horizon)[
  #set text(size: 35pt)
  = But like, isn't this kinda useless
  ]

  #show: later
  #align(center+bottom)[
  ...well yeah, the final result kinda is ;-;\
  ...but the individual components aren't!
  ]
]


#slide[
  #align(horizon)[
  #grid(columns:(auto, auto), gutter: 5pt,[
      #set text(size: 20pt)
      = The silver lining
      #v(1em)
      #set text(size: 18pt)
      #align(horizon)[
      - The $clam$-calculus itself is extremely important!
      - Many functional programming languages (Haskell, Lean, etc) have it at their very core!
      ]
    ],align(bottom+right)[
      // #show: later
      #image("images/fpmentioned.jpg", height: 100%)
    ]
  )
  ]
]

#slide[
  #set page(margin:5pt)
  #align(center+horizon)[
  #set text(size: 18pt)
    #grid(columns:(35%,35%),
    gutter:5pt,
    [
      #set align(top)
      #image("images/slides-qrcode.png", height:50%)
      Slide repository
    ],
    [
      #set align(top)
      #image("images/challenge-qrcode.png", height:50%)
      The riddle from my team's previous presentation
    ]
    )

  #set text(size: 35pt)
  ✨ Thanks You! ✨
  ]
]

#slide[ #show:later ]
