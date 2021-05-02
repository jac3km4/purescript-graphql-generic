{ name = "graphql-generic"
, dependencies =
    [ "effect"
    , "prelude"
    , "record"
    , "psci-support"
    -- test deps
    , "spec"
    , "aff"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
