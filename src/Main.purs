module Main where

import Prelude
import PureScript.CST.Types
import Tidy.Codegen
import Tidy.Codegen.Monad

import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

exampleModule :: Module Void
exampleModule = unsafePartial $ codegenModule "Data.Maybe" do
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
personModule = unsafePartial $ codegenModule "Person" do
  tell 
    [
      declType "Person" [] ( typeRecord
              [ Tuple "name" (typeCtor "String")
              , Tuple "age" (typeCtor "Int")
              ]
              Nothing
          )
    , declForeign "getAge" ( typeArrow
                      [ typeCtor "Person"                      ]
                      ( typeCtor "Number" 
                      ))
    ]

main :: Effect Unit
main = do
  log (printModule personModule)
