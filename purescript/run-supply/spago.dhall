{ name = "run-supply"
, dependencies = [ "maybe", "prelude", "run", "tuples", "typelevel-prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "GPL3"
, repository = "https://github.com/Mateiadrielrafael/purescript-run-supply"
}
