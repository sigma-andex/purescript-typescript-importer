module Typescript.Parser where

import Prelude
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Record.Builder (merge)
import Type.Row (type (+))

foreign import data SourceFile :: Type

foreign import data Program :: Type

foreign import createProgram :: Array String -> Effect Program

foreign import createSourceFile :: String -> String -> Effect SourceFile

foreign import getSourceFile :: Program -> String -> Effect SourceFile

foreign import getSourceFiles :: Program -> Effect (Array SourceFile)

foreign import getSourceFileName :: SourceFile -> String

foreign import getSourceFileChildren :: forall r. SourceFile -> Array (Record ( | BaseNode r ))

foreign import getChildren :: forall r. Record ( | BaseNode r ) -> Array (Record ( | BaseNode r ))

foreign import isTypeAliasDeclarationImpl :: forall r. Record ( | BaseNode + r ) -> Nullable (Record ( | TypeAliasDeclaration + r ))

isTypeAliasDeclaration :: forall r. Record ( | BaseNode + r ) -> Maybe (Record ( | TypeAliasDeclaration + r ))
isTypeAliasDeclaration = isTypeAliasDeclarationImpl >>> toMaybe

type Identifier r = ( text :: String | r )

type BaseSymbol = {}

type TransientIdentifier r = ( resolvedSymbol :: BaseSymbol | Identifier + r )

type BaseNode r
  = ( | r )

type BaseTypeNode r
  = ( | BaseNode + r )

type BaseTypeElement r
  = ( | r )

type BaseDeclaration r
  = ( | BaseNode + r )
type BaseNamedDeclaration name r
  = ( name :: name | BaseDeclaration + r )
type BaseDeclarationStatement name r
  = ( | BaseNamedDeclaration name r )

type BaseTypeAliasDeclaration name r
  = ( | BaseDeclarationStatement name r )

type TypeAliasDeclaration r = ( | BaseTypeAliasDeclaration { | Identifier ()} r )

type BaseTypeLiteralNode members r
  = ( members :: Array (BaseTypeElement members) | BaseTypeNode + BaseDeclaration + r )

type BaseSignatureDeclarationBase tpe r
  = ( "type" :: Record (BaseTypeNode tpe) | r )

type BaseFunctionLikeDeclaration tpe r
  = ( | BaseSignatureDeclarationBase tpe + r )

type BaseFunctionDeclaration tpe r
  = ( | BaseFunctionLikeDeclaration tpe + r )
