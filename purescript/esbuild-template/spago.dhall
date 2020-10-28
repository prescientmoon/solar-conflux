{ name = "Purescript project"
, dependencies =
  [ "prelude", "psci-support", "generics-rep", "debugged", "effect", "console" ]
, sources = [ "src/**/*.purs" ]
, packages = ./packages.dhall
}
