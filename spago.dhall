{ name = "purescript-typescript-importer"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "language-cst-parser"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "partial"
  , "prelude"
  , "psci-support"
  , "record"
  , "strings"
  , "tidy-codegen"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "generated-src/**/*.purs" ]
}
