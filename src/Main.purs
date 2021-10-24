module Main where

import Prelude
import PureScript.CST.Types
import Tidy.Codegen
import Tidy.Codegen.Monad

import Control.Monad.Writer (tell)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as S
import Node.Path as Path
import Partial.Unsafe (unsafePartial)
import Person (getAge)
import Typescript.Parser (createProgram, createSourceFile, getSourceFile)

exampleModule :: Module Void
exampleModule =
  unsafePartial
    $ codegenModule "Data.Maybe" do
        importOpen "Prelude"
        tell
          [ declData "Maybe" [ typeVar "a" ]
              [ dataCtor "Nothing" []
              , dataCtor "Just" [ typeVar "a" ]
              ]
          , declSignature "maybe" do
              typeForall [ typeVar "a", typeVar "b" ] do
                typeArrow
                  [ typeVar "b"
                  , typeArrow [ typeVar "a" ] (typeVar "b")
                  , typeApp (typeCtor "Maybe") [ typeVar "a" ]
                  ]
                  (typeVar "b")
          , declValue "maybe" [ binderVar "nothing", binderVar "just" ] do
              exprCase [ exprSection ]
                [ caseBranch [ binderCtor "Just" [ binderVar "a" ] ] do
                    exprApp (exprIdent "just") [ exprIdent "a" ]
                , caseBranch [ binderCtor "Nothing" [] ] do
                    exprIdent "nothing"
                ]
          ]

personModule :: Module Void
personModule =
  unsafePartial
    $ codegenModule "Person" do
        tell
          [ declType "Person" []
              ( typeRecord
                  [ Tuple "name" (typeCtor "String")
                  , Tuple "age" (typeCtor "Int")
                  ]
                  Nothing
              )
          , declForeign "getAge"
              ( typeArrow
                  [ typeCtor "Person" ]
                  ( typeCtor "Number"
                  )
              )
          ]



dirs :: Effect (Array String)
dirs = S.readdir "person" <#> filter (contains (Pattern "ts"))

tsCompilerExample :: String -> String -> Effect Unit 
tsCompilerExample name sourceFile = do 
  sf <- createSourceFile name sourceFile 
  let 
    _ = spy "sourcefile" sf 
  pure unit


main :: Effect Unit
main = do
  log $ show (getAge { name : "Franz", age : 1242})
  log "-----"
  files <- dirs
  program <- createProgram $ spy "files" files
  sourceFile <- getSourceFile program "person.ts"
  log "loaded"
