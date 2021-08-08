{ name = "run-external-state"
, dependencies =
  [ "maybe"
  , "prelude"
  , "effect"
  , "refs"
  , "profunctor-lenses"
  , "run"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "GPL-3.0-or-later"
, repository = "https://github.com/Mateiadrielrafael/purescript-run-supply"
}
