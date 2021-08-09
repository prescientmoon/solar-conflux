# External state

Alternative representation for `State`, allowing for some new interpreters,

Usecases:

- conccurrent state
- saving the state inside a `Ref`

For the full documentation check out [pursuit](https://pursuit.purescript.org/packages/purescript-run-external-state/1.0.0)

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
