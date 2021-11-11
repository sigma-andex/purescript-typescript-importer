module Typescript.Parser where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy)
import Typescript.SyntaxKind as SK

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

type TypeNodeR r = (kind :: Proxy SK.SyntaxKindTList | r)
type TypeNode
  = { | TypeNodeR () }

type KeywordSyntaxKind
  = SK.StringKeyword :> SK.NumberKeyword :> Nil'

type PropertyName
  = Identifier

type TypeElement =
  { name :: Nullable PropertyName
  }

type PropertySignature
  =
  { kind :: Proxy (SK.PropertySignature :> Nil')
  , name :: PropertyName
  , "type" :: Nullable TypeNode
  }

type TypeAliasDeclaration
  =
  { "kind" :: Proxy (SK.TypeAliasDeclaration :> Nil')
  , name :: Identifier
  , "type" :: TypeNode
  }

type KeywordToken =
  { "kind" :: Proxy KeywordSyntaxKind
  }

type TypeLiteralNode
  =
  { "kind" :: Proxy (SK.TypeLiteral :> Nil')
  , members :: Array TypeNode
  }

type ParameterDeclaration =
  { kind :: Proxy (SK.Parameter :> Nil')
  , name :: Identifier
  , type :: Nullable TypeNode
  }

type FunctionDeclaration
  =
  { kind :: Proxy (SK.FunctionDeclaration :> Nil')
  , name :: Nullable Identifier
  , parameters :: Array ParameterDeclaration
  , type :: Nullable TypeNode
  }

type EntityName = Identifier

type TypeReferenceNode
  =
  { kind :: Proxy (SK.TypeReference :> Nil')
  , typeName :: EntityName
  }