{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "psci-support", "other" ]
, packages = ./packages.dhall
, sources = [ "src/Maain.purs" ]
}
