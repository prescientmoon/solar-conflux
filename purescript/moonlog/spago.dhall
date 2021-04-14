{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "fixed-points"
  , "matryoshka"
  , "node-fs"
  , "node-fs-aff"
  , "node-readline"
  , "nullable"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "run"
  , "strings"
  , "stringutils"
  , "undefined"
  , "unordered-collections"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
