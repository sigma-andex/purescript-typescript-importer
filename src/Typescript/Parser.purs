module Typescript.Parser where

import Data.Maybe (Maybe)
import Effect (Effect)
import Record.Builder (merge)

foreign import data SourceFile :: Type

foreign import data Program :: Type

foreign import data Identifier :: Type

foreign import data Node :: Type 

foreign import createProgram :: Array String -> Effect Program

foreign import createSourceFile :: String -> String -> Effect SourceFile

foreign import getSourceFile :: Program -> String -> Effect SourceFile

foreign import getSourceFiles :: Program -> Effect (Array SourceFile)

foreign import getSourceFileName :: SourceFile -> String

foreign import getSourceFileChildren :: SourceFile -> Array Node

foreign import getChildren :: Node -> Array Node

foreign import getName :: Node -> Identifier

foreign import getText :: Identifier -> String

