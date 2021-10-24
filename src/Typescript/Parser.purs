module Typescript.Parser where

import Effect (Effect)

foreign import data SourceFile :: Type 

foreign import data Program :: Type 

foreign import createProgram :: Array String -> Effect Program

foreign import createSourceFile :: String -> String -> Effect SourceFile

foreign import getSourceFile :: Program -> String -> Effect SourceFile