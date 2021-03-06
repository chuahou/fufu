{ name = "fufu"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "foldable-traversable"
    , "maybe"
    , "psci-support"
    , "random"
    , "web-dom"
    , "web-html"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
