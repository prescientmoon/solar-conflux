{ name = "run-supply"
, dependencies = [ "maybe", "prelude", "run", "tuples", "typelevel-prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
