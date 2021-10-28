module Main where

import Prelude

import Control.Monad.Writer (tell)
import Data.Array (filter)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync as S
import Partial.Unsafe (unsafePartial)
import Person (getAge)
import PureScript.CST.Types (Module)
import Tidy.Codegen (binderCtor, binderVar, caseBranch, dataCtor, declData, declForeign, declSignature, declType, declValue, exprApp, exprCase, exprIdent, exprSection, printModule, typeApp, typeArrow, typeCtor, typeForall, typeRecord, typeVar)
import Tidy.Codegen.Monad (codegenModule, importOpen)
import Typescript.Parser (createProgram, createSourceFile, getChildren, getSourceFile, getSourceFileChildren, isTypeAliasDeclaration)

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
dirs = S.readdir "person" <#> filter (contains (Pattern "ts")) <#> map (\n -> "person/" <> n)

tsCompilerExample :: String -> String -> Effect Unit
tsCompilerExample name sourceFile = do
  sf <- createSourceFile name sourceFile
  let
    _ = spy "sourcefile" sf
  pure unit

main :: Effect Unit
main = do
  log "----- Purescript code gen"
  log $ printModule personModule
  log $ show (getAge { name: "Franz", age: 1242 })
  log "----- Typescript file loading"
  files <- dirs
  log $ show files
  program <- createProgram $ spy "files" files
  sourceFile <- getSourceFile program "person/person.ts"
  for_ ((getSourceFileChildren >=> getChildren) sourceFile) \n -> do
    -- let 
    --   _ = spy "node" n 
    -- log ""
    log
      $ case isTypeAliasDeclaration n of
          Just tln -> 
              "Got a type alias declaration: " <> tln.name.text
          Nothing -> "Not a type alias declaration!"
  log "loaded"
