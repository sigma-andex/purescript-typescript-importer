{ name = "purescript-typescript-importer"
, dependencies =
  [ "console"
  , "effect"
  , "language-cst-parser"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "tidy-codegen"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
