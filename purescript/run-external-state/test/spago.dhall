{ name = "run-supply"
, dependencies = [ "maybe", "prelude", "run", "tuples", "typelevel-prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "GPL-3.0-or-later"
, repository = "https://github.com/Mateiadrielrafael/purescript-run-supply"
}
