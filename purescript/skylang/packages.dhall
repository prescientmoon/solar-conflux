let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220127/packages.dhall
        sha256:8ccbd53dbc7dbfd92a9cba9cca7a8bf36cb120a0a3e21106bf19a16d3ad6863e

let additions =
      { run-supply =
        { repo = "https://github.com/Mateiadrielrafael/purescript-run-supply"
        , version = "585c281"
        , dependencies =
          [ "maybe", "prelude", "run", "tuples", "typelevel-prelude" ]
        }
      , debugged =
        { repo = "https://github.com/Mateiadrielrafael/purescript-debugged/"
        , version = "633220f91f87c9acbc4eebbf87628e6cdc658b7b"
        , dependencies =
          [ "prelude"
          , "console"
          , "ordered-collections"
          , "either"
          , "tuples"
          , "lists"
          , "strings"
          , "arrays"
          , "bifunctors"
          , "record"
          , "effect"
          , "datetime"
          , "enums"
          , "unordered-collections"
          , "fixed-points"
          , "foldable-traversable"
          , "math"
          , "maybe"
          , "newtype"
          , "partial"
          ]
        }
      }

in  upstream // additions
