{ name = "purebird"
, dependencies =
  [ "arrays"
  , "canvas"
  , "console"
  , "effect"
  , "prelude"
  , "psci-support"
  , "random"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
