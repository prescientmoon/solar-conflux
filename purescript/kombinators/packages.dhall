let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210825/packages.dhall
        sha256:eee0765aa98e0da8fc414768870ad588e7cada060f9f7c23c37385c169f74d9f

let additions =
      { run-supply =
        { dependencies =
          [ "maybe", "prelude", "run", "tuples", "typelevel-prelude" ]
        , repo = "https://github.com/Mateiadrielrafael/purescript-run-supply/"
        , version = "585c281c8e631816246b7bb3c653c7beba85b490"
        }
      , debugged =
        { dependencies =
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
          ]
        , repo = "https://github.com/Mateiadrielrafael/purescript-debugged"
        , version = "633220f91f87c9acbc4eebbf87628e6cdc658b7b"
        }
      }

in  upstream // additions
