{ name = "purescript-typescript-importer"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "language-cst-parser"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "nullable"
  , "partial"
  , "prelude"
  , "psci-support"
  , "record"
  , "strings"
  , "tidy-codegen"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "generated-src/**/*.purs" ]
}
