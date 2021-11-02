module Typescript.Parser where

import Prelude
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Type.Proxy (Proxy)
import Type.Row (type (+))
import Typescript.Utils.Enum (class Enum, class EnumConfig)
import Untagged.Union (type (|+|))

foreign import data SourceFile :: Type

foreign import data Program :: Type

foreign import createProgram :: Array String -> Effect Program

foreign import createSourceFile :: String -> String -> Effect SourceFile

foreign import getSourceFile :: Program -> String -> Effect SourceFile

foreign import getSourceFiles :: Program -> Effect (Array SourceFile)

foreign import getSourceFileName :: SourceFile -> String

foreign import getSourceFileChildren :: forall r. SourceFile -> Array (Record ( | BaseNode r ))

foreign import getChildren :: forall r. Record ( | BaseNode r ) -> Array (Record ( | BaseNode r ))

foreign import isTypeAliasDeclarationImpl :: forall typeNode r. Record ( | BaseNode + r ) -> Nullable (Record ( | TypeAliasDeclaration typeNode + r ))

isTypeAliasDeclaration :: forall typeNode r. Record ( | BaseNode + r ) -> Maybe (Record ( | TypeAliasDeclaration typeNode + r ))
isTypeAliasDeclaration = isTypeAliasDeclarationImpl >>> toMaybe

foreign import isTypeLiteralNodeImpl :: forall r. Record ( | BaseNode + r ) -> Nullable (Record TypeLiteralNode)

isTypeLiteralNode :: forall r. Record ( | BaseNode + r ) -> Maybe (Record TypeLiteralNode)
isTypeLiteralNode = isTypeLiteralNodeImpl >>> toMaybe

foreign import isPropertySignatureImpl :: forall typeNode r. Record ( | BaseTypeNode + r ) -> Nullable (Record ( | PropertySignature typeNode + r ))

isPropertySignature :: forall typeNode r. Record ( | BaseTypeNode + r ) -> Maybe (Record ( | PropertySignature typeNode + r ))
isPropertySignature = isPropertySignatureImpl >>> toMaybe

type Identifier r
  = ( text :: String | r )

type BaseSymbol
  = {}

type TransientIdentifier r
  = ( resolvedSymbol :: BaseSymbol | Identifier + r )

type BaseNode :: forall k. k -> k
type BaseNode r
  = ( | r )

type BaseTypeNode :: forall k. Row k -> Row k
type BaseTypeNode r
  = ( | BaseNode + r )

type BaseTypeElement :: forall k. k -> k
type BaseTypeElement r
  = ( | r )

type TypeElement :: forall k. Row k
type TypeElement
  = BaseTypeElement ()

data SyntaxKindEnum

foreign import data StringKeyword :: SyntaxKindEnum
foreign import data NumberKeyword :: SyntaxKindEnum

data SyntaxKind :: SyntaxKindEnum -> Type
data SyntaxKind k

foreign import numberKeyword :: SyntaxKind NumberKeyword
foreign import stringKeyword :: SyntaxKind StringKeyword

instance Enum SyntaxKind NumberKeyword StringKeyword where
  enumValue = numberKeyword
instance Enum SyntaxKind StringKeyword Unit where
  enumValue = stringKeyword

instance EnumConfig SyntaxKindEnum SyntaxKind NumberKeyword

type BaseToken :: forall k. k -> Row Type -> Row Type
type BaseToken kind r
  = BaseNode ( kind :: Proxy kind | r )

type Token :: Type -> Row Type -> Row Type
type Token kind r
  = BaseToken kind r

type KeywordSyntaxKind
  = SyntaxKind StringKeyword |+| SyntaxKind NumberKeyword
type KeywordToken r
  = BaseToken KeywordSyntaxKind r

type PropertyName
  = Identifier ()

type PropertySignature typeNode r
  = ( name :: { | PropertyName }, type :: Nullable { | BaseTypeNode typeNode } | r )

type BaseDeclaration :: forall k. Row k -> Row k
type BaseDeclaration r
  = ( | BaseNode + r )

type BaseNamedDeclaration :: forall k. k -> Row k -> Row k
type BaseNamedDeclaration name r
  = ( name :: name | BaseDeclaration + r )

type BaseDeclarationStatement :: forall k. k -> Row k -> Row k
type BaseDeclarationStatement name r
  = ( | BaseNamedDeclaration name r )

type BaseTypeAliasDeclaration :: forall k. k -> Row k -> Row k
type BaseTypeAliasDeclaration name r
  = ( | BaseDeclarationStatement name r )

type TypeAliasDeclaration typeNode r
  = ( "type" :: { | BaseTypeNode typeNode } | BaseTypeAliasDeclaration { | Identifier () } r )

type TypeLiteralNode
  = ( members :: Array (Record TypeElement) | BaseTypeNode + BaseDeclaration + () )

type BaseSignatureDeclarationBase tpe r
  = ( "type" :: Record (BaseTypeNode tpe) | r )

type BaseFunctionLikeDeclaration tpe r
  = ( | BaseSignatureDeclarationBase tpe + r )

type BaseFunctionDeclaration tpe r
  = ( | BaseFunctionLikeDeclaration tpe + r )
