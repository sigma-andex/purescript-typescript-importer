{ name = "typescript-importer"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "functions"
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
  , "typescript-utils"
  , "unfoldable"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "generated-src/**/*.purs" ]
, license = "MIT-0"
, repository =
    "https://github.com/sigma-andex/purescript-typescript-importer.git"
}
