# Supply

An effect useful for generating an infinite supply of values.

## Example

```purescript
-- Generates 2 values and adds them together
foo :: forall r. Run (SUPPLY Int r) Int
foo = ado
    a <- generate
    b <- generate
    in a + b

-- | Runs `foo` by providing 2 as the starting value and adding 3 to each subsequent call.
runnedFoo :: Int
runnedFoo
    = foo -- Run (SUPPLY Int ()) Int
    # runSupply
        ((+) 3) -- generates the next value
        2 -- initial value
    # extract -- Run () Int -> Int
```

For the full documentation check out [pursuit](TODO)

## Development

Building the package

```
spago build
```

Running the test suite

```
spago -x ./spago.test.dhall test
```

If you think a particular helper would be an useful addition, feel free to open an issue.
