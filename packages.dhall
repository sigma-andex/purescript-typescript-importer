let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be
    with language-cst-parser = { dependencies =
          [ "arrays"
          , "const"
          , "control"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "free"
          , "functors"
          , "identity"
          , "integers"
          , "lazy"
          , "lists"
          , "maybe"
          , "newtype"
          , "numbers"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "psci-support"
          , "st"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "unfoldable"
          , "unsafe-coerce"
          ]
      , repo =
          "https://github.com/reactormonk/purescript-language-cst-parser.git"
      , version =
          "b5e42a89b68cb42c1fce68d7165226879cb1951c"
          }
    with tidy = { dependencies =
          [ "arrays"
          , "control"
          , "dodo-printer"
          , "either"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "language-cst-parser"
          , "strings"
          , "tuples"
          ]
      , repo =
          "https://github.com/natefaubion/purescript-tidy.git"
      , version =
          "v0.6.0"
          }
    with tidy-codegen =
      { dependencies =
          [ "aff"
          , "ansi"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "console"
          , "control"
          , "dodo-printer"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "filterable"
          , "foldable-traversable"
          , "free"
          , "identity"
          , "integers"
          , "language-cst-parser"
          , "lazy"
          , "lists"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "node-child-process"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "node-streams"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "posix-types"
          , "prelude"
          , "record"
          , "safe-coerce"
          , "strings"
          , "tidy"
          , "transformers"
          , "tuples"
          , "type-equality"
          , "unicode"
          ]
      , repo =
          "https://github.com/natefaubion/purescript-tidy-codegen.git"
      , version =
          "v1.1.1"
      }
      with typescript-utils =
      { dependencies =
          [ "console"
          , "effect"
          , "functions"
          , "prelude"
          , "psci-support"
          , "typelevel-lists"
          , "unsafe-coerce"
          ]
      , repo =
          "https://github.com/sigma-andex/purescript-typescript-utils.git"
      , version =
          "main"
      }
in  upstream
