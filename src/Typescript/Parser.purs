module Typescript.Parser where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Type.Proxy (Proxy)
import Typescript.SyntaxKind as SK
import Untagged.Union (type (|+|))

foreign import data SourceFile :: Type

foreign import data Program :: Type

foreign import createProgram :: Array String -> Effect Program

foreign import createSourceFile :: String -> String -> Effect SourceFile

foreign import getSourceFile :: Program -> String -> Effect SourceFile

foreign import getSourceFiles :: Program -> Effect (Array SourceFile)

foreign import getSourceFileName :: SourceFile -> String

foreign import getSourceFileChildren :: SourceFile -> Array Node

foreign import getChildren :: Node -> Array Node

foreign import isTypeAliasDeclarationImpl :: forall r. { | NodeR r } -> Nullable TypeAliasDeclaration

isTypeAliasDeclaration :: forall r. { | NodeR r } -> Maybe TypeAliasDeclaration
isTypeAliasDeclaration = isTypeAliasDeclarationImpl >>> toMaybe

foreign import isTypeLiteralNodeImpl :: forall r. { | NodeR r } -> Nullable TypeLiteralNode

isTypeLiteralNode :: forall r. { | NodeR r } -> Maybe TypeLiteralNode
isTypeLiteralNode = isTypeLiteralNodeImpl >>> toMaybe

foreign import isPropertySignatureImpl :: forall r. { | NodeR r } -> Nullable PropertySignature

isPropertySignature :: forall r. { | NodeR r } -> Maybe PropertySignature
isPropertySignature = isPropertySignatureImpl >>> toMaybe

foreign import isFunctionDeclarationImpl :: forall r. { | NodeR r } -> Nullable FunctionDeclaration

isFunctionDeclaration :: forall r. { | NodeR r } -> Maybe FunctionDeclaration
isFunctionDeclaration = isFunctionDeclarationImpl >>> toMaybe

foreign import isTypeReferenceNodeImpl :: forall r. { | NodeR r } -> Nullable TypeReferenceNode

isTypeReferenceNode :: forall r. { | NodeR r } -> Maybe TypeReferenceNode
isTypeReferenceNode = isTypeReferenceNodeImpl >>> toMaybe

type Identifier
  = { text :: String }

type NodeR :: forall k. k -> k
type NodeR r = (| r)

type Node
  = { | NodeR () }

type TypeNodeR r = (kind :: SK.SyntaxKindEnum | r)
type TypeNode
  = { | TypeNodeR () }

type KeywordSyntaxKind
  = SK.SyntaxKind SK.StringKeyword |+| SK.SyntaxKind SK.NumberKeyword

type PropertyName
  = Identifier

type TypeElement =
  { name :: Nullable PropertyName
  }

type PropertySignature
  =
  { kind :: SK.SyntaxKind SK.PropertySignature
  , name :: PropertyName
  , "type" :: Nullable TypeNode
  }

type TypeAliasDeclaration
  =
  { "kind" :: SK.SyntaxKind SK.TypeAliasDeclaration
  , name :: Identifier
  , "type" :: TypeNode
  }

type KeywordToken =
  { "kind" :: Proxy KeywordSyntaxKind
  }

type TypeLiteralNode
  =
  { "kind" :: SK.SyntaxKind SK.TypeLiteral
  , members :: Array TypeNode
  }

type ParameterDeclaration =
  { kind :: SK.SyntaxKind SK.Parameter
  , name :: Identifier
  , type :: Nullable TypeNode
  }

type FunctionDeclaration
  =
  { kind :: SK.SyntaxKind SK.FunctionDeclaration
  , name :: Nullable Identifier
  , parameters :: Array ParameterDeclaration
  , type :: Nullable TypeNode
  }

type EntityName = Identifier

type TypeReferenceNode
  =
  { kind :: SK.SyntaxKind SK.TypeReference
  , typeName :: EntityName
  }