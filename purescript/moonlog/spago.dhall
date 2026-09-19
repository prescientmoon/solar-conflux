{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "fixed-points"
  , "foldable-traversable"
  , "lists"
  , "matryoshka"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-readline"
  , "nullable"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "run"
  , "strings"
  , "stringutils"
  , "tuples"
  , "typelevel-prelude"
  , "undefined"
  , "unordered-collections"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
