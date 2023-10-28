# Abilities

Implementation of my idea of abusing typeclass-dictionaries in order to create a barebones effect system.

## File structure

| File                                   | Description                                                      |
| -------------------------------------- | ---------------------------------------------------------------- |
| [Abilities.purs](./src/Abilities.purs) | Overloaded pure do-notation enabling implicit dictionary passing |
| [Ask.purs](./src/Ask.purs)             | Reader-monad effect                                              |
| [Io.purs](./src/Ask.purs)              | IO-monad effect                                                  |
| [Main.purs](./src/Main.purs)           | Demo showcasing usage of `IO` together with `Ask`                |
