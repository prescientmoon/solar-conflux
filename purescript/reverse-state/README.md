# Reverse state

My experiments with reverse state in purescript. Here's a short proof of concept

```purs
do
    future <- get -- 4
    modify \a -> a * 2
    future' <- get -- 2
    put 2
```
