module Typescript.Parser
  ( EntityName
  , FunctionDeclaration
  , Identifier
  , KeywordSyntaxKind
  , KeywordToken
  , ModuleBlock
  , ModuleBody
  , ModuleDeclaration
  , NamespaceBody
  , Node
  , NodeR
  , ParameterDeclaration
  , Program
  , PropertyName
  , PropertySignature
  , SourceFile
  , Statement
  , TypeAliasDeclaration
  , TypeElement
  , TypeLiteralNode
  , TypeNode
  , TypeNodeR
  , TypeReferenceNode
  , VariableDeclaration
  , VariableDeclarationList
  , VariableStatement
  , createProgram
  , createSourceFile
  , getChildren
  , getSourceFile
  , getSourceFileChildren
  , getSourceFileName
  , getSourceFiles
  , isFunctionDeclaration
  , isModuleBlock
  , isModuleDeclaration
  , isPropertySignature
  , isTypeAliasDeclaration
  , isTypeLiteralNode
  , isTypeReferenceNode
  , isVariableStatement
  ) where

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

foreign import isVariableStatementImpl :: forall r. { | NodeR r } -> Nullable VariableStatement

isVariableStatement :: forall r. { | NodeR r } -> Maybe VariableStatement
isVariableStatement = isVariableStatementImpl >>> toMaybe

-- function isModuleDeclaration(node: Node): node is ModuleDeclaration;
-- function isModuleBlock(node: Node): node is ModuleBlock;
foreign import isModuleDeclarationImpl :: forall r. { | NodeR r } -> Nullable ModuleDeclaration

isModuleDeclaration :: forall r. { | NodeR r } -> Maybe ModuleDeclaration
isModuleDeclaration = isModuleDeclarationImpl >>> toMaybe

foreign import isModuleBlockImpl :: forall r. { | NodeR r } -> Nullable ModuleBlock

isModuleBlock :: forall r. { | NodeR r } -> Maybe ModuleBlock
isModuleBlock = isModuleBlockImpl >>> toMaybe

type Identifier
  = { text :: String }

type NodeR r = (kind :: SK.SyntaxKindEnum | r)

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

type PropertySignature =
  { kind :: SK.SyntaxKind SK.PropertySignature
  , name :: PropertyName
  , questionToken :: Nullable {}
  , type :: Nullable TypeNode
  }

type TypeAliasDeclaration =
  { "kind" :: SK.SyntaxKind SK.TypeAliasDeclaration
  , name :: Identifier
  , "type" :: TypeNode
  }

type KeywordToken =
  { "kind" :: Proxy KeywordSyntaxKind
  }

type TypeLiteralNode =
  { "kind" :: SK.SyntaxKind SK.TypeLiteral
  , members :: Array TypeNode
  }

type ParameterDeclaration =
  { kind :: SK.SyntaxKind SK.Parameter
  , name :: Identifier
  , type :: Nullable TypeNode
  , questionToken :: Nullable {}
  }

type FunctionDeclaration =
  { kind :: SK.SyntaxKind SK.FunctionDeclaration
  , name :: Nullable Identifier
  , parameters :: Array ParameterDeclaration
  , type :: Nullable TypeNode
  }

type EntityName = Identifier

type TypeReferenceNode =
  { kind :: SK.SyntaxKind SK.TypeReference
  , typeName :: EntityName
  }

type VariableDeclaration =
  { name :: Identifier
  , type :: Nullable TypeNode
  }

type VariableDeclarationList =
  { kind :: SK.SyntaxKind SK.VariableDeclarationList
  , declarations :: Array VariableDeclaration
  }

type VariableStatement =
  { kind :: SK.SyntaxKind SK.VariableStatement
  , declarationList :: VariableDeclarationList
  }

type Statement = Node
type ModuleBlock =
  { kind :: SK.SyntaxKind SK.ModuleBlock
  , statements :: Array Statement
  }

type NamespaceBody = ModuleBlock
type ModuleBody = NamespaceBody
type ModuleDeclaration =
  { kind :: SK.SyntaxKind SK.ModuleDeclaration
  , name :: Identifier
  , body :: Nullable ModuleBlock
  }
