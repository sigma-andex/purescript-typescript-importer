module GenCode where

import Prelude

import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable as Unfoldable
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types as CST
import Tidy.Codegen (declForeign, declType, printModule, typeArrow, typeCtor, typeRecord)
import Tidy.Codegen.Monad (codegenModule)
import Type.Proxy (Proxy)
import Type.Row (type (+))
import Typescript.Parser as TS
import Typescript.SyntaxKind as SK
import Typescript.Utils.Enum (default, on)

constJust :: forall a b. a -> b -> Maybe a
constJust = Just >>> const

parseTypeNode :: ∀ n e. Partial => { | TS.TypeNodeR + n } -> Maybe (CST.Type e)
parseTypeNode n@{ kind } = 
        let
          parseMember :: TS.TypeNode -> Maybe (Tuple String (CST.Type e))
          parseMember memberNode = do
            ps <- TS.isPropertySignature memberNode
            let
              name = ps.name.text
              tpe = ps."type" # toMaybe >>= parseTypeNode
            tl <- tpe
            Just $ Tuple name tl

          parseTypeLiteralNode :: { | TS.TypeNodeR + n } -> Maybe (CST.Type e)
          parseTypeLiteralNode tn = TS.isTypeLiteralNode tn <#> \tln -> typeRecord (toMembers tln.members) Nothing
            where 
              toMembers :: Array TS.TypeNode -> Array (Tuple String (CST.Type e))
              toMembers tn = tn <#> parseMember >>= Unfoldable.fromMaybe
        
          parseTypeReference :: { | TS.TypeNodeR + n } -> Maybe (CST.Type e)
          parseTypeReference tn = TS.isTypeReferenceNode tn <#> \ref -> typeCtor ref.typeName.text

          caseFn :: Proxy SK.SyntaxKindTList -> Maybe (CST.Type e)
          caseFn = 
            default (Nothing :: Maybe (CST.Type e))
              # on SK.typeLiteral (const (parseTypeLiteralNode n))
              # on SK.typeReference (const (parseTypeReference n))
              # on SK.numberKeyword (constJust $ typeCtor "Number")
              # on SK.stringKeyword (constJust $ typeCtor "String")
        in caseFn kind


parseNode :: ∀ n e. Partial => { | TS.NodeR + n } -> Maybe (CST.Declaration e)
parseNode n = case TS.isTypeAliasDeclaration n of 
  Just tad -> do
    tpe <- parseTypeNode tad."type"
    pure $ declType tad.name.text [] tpe
  Nothing -> do
    fd <- TS.isFunctionDeclaration n
    name <- toMaybe fd.name
    tpe <- toMaybe fd."type" >>= parseTypeNode
    let
      params :: Array (Maybe TS.TypeNode)
      params = fd.parameters <#> (_."type" >>> toMaybe)
    members <- sequence $ params <#> (\mp -> mp >>= parseTypeNode)
    let
      fn =  declForeign name.text ( typeArrow
                      members
                      tpe)
    Just fn
  

genCode :: Array String -> Effect (Array String)
genCode fileNames = do
  program <- TS.createProgram fileNames
  sourceFiles <- traverse (TS.getSourceFile program) fileNames

  traverse generateOne (sourceFiles)
  where
  generateOne sf = do
    let
      generatedModule = unsafePartial $ codegenModule "Person" do -- [TODO] Get real pascal name
        let declarations = TS.getSourceFileChildren sf >>= TS.getChildren <#> parseNode >>= Unfoldable.fromMaybe
        tell declarations
    pure $ printModule generatedModule
