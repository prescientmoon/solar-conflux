module My.Syntax.Rewrite

infix  1  ....

public export
(....) : (0 a : Type) -> a -> a
(....) _ a = a

public export
… : (0 a : Type) -> a -> a
… _ a = a

public export
…l : (0 a : t) -> {0 b : t} -> a = b -> a = b
…l _ a = a

public export
…r : {0 a : t} -> (0 b : t) -> a = b -> a = b
…r _ a = a
