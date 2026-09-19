let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210516/packages.dhall sha256:f5e978371d4cdc4b916add9011021509c8d869f4c3f6d0d2694c0e03a85046c8

let additions =
      { node-readline-aff =
        { dependencies =
          [ "prelude"
          , "console"
          , "psci-support"
          , "node-readline"
          , "aff"
          , "node-streams"
          , "options"
          , "exceptions"
          , "either"
          ]
        , repo = "https://github.com/ChrisPenner/purescript-node-readline-aff"
        , version = "master"
        }
      , options =
        { dependencies =
          [ "assert"
          , "console"
          , "contravariant"
          , "effect"
          , "foreign"
          , "foreign-object"
          , "maybe"
          , "psci-support"
          , "tuples"
          ]
        , repo = "https://github.com/purescript-contrib/purescript-options"
        , version = "main"
        }
      }

in  upstream // additions
