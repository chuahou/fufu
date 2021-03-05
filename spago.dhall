{ name = "fufu"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "foldable-traversable"
    , "maybe"
    , "psci-support"
    , "random"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
