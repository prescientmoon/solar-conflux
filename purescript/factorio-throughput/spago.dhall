{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "filterable"
  , "profunctor-lenses"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "run"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "strings"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
