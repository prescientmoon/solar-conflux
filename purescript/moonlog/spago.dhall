{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "fixed-points"
  , "matryoshka"
  , "nullable"
  , "psci-support"
  , "run"
  , "undefined"
  , "unordered-collections"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
