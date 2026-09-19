let main = ./spago.dhall

in  { name = "run-external-state-tests"
    , dependencies = main.dependencies # [ "spec", "aff", "integers" ]
    , sources = main.sources # [ "test/**/*.purs" ]
    , packages = ./packages.dhall
    }
