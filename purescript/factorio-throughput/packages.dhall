let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210324/packages.dhall sha256:b4564d575da6aed1c042ca7936da97c8b7a29473b63f4515f09bb95fae8dddab

let additions =
      { debugged =
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
          ]
        , repo = "https://github.com/hdgarrood/purescript-debugged"
        , version = "master"
        }
      }

in  upstream // additions
