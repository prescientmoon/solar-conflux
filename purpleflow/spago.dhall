{ name = "purpleflow"
, dependencies =
  [ "codec"
  , "console"
  , "effect"
  , "prelude"
  , "run"
  , "strings"
  , "transformers"
  , "unicode"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "packages/**/*.purs", "test/**/*.purs" ]
}
