let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115

let additions =
      { zipperarray =
        { dependencies =
          [ "arrays", "maybe", "prelude", "naturals", "strictlypositiveint" ]
        , repo = "https://github.com/jamieyung/purescript-zipperarray/"
        , version = "master"
        }
      , strictlypositiveint =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/jamieyung/purescript-strictlypositiveint"
        , version = "master"
        }
      }

in  additions ⫽ upstream
