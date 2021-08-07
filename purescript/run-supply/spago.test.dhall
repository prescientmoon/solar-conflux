let main = ./spago.dhall

in  { name = "run-supply-tests"
    , dependencies = main.dependencies # [ "effect", "spec", "aff", "integers" ]
    , sources = main.sources # [ "test/**/*.purs" ]
    , packages = ./packages.dhall
    }
