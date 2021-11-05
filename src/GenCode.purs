module GenCode where

import Prelude

import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable as Unfoldable
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types as CST
import Tidy.Codegen (declType, printModule, typeCtor, typeRecord)
import Tidy.Codegen.Monad (codegenModule)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Typescript.Parser (BaseNode, SyntaxKindEnum, createProgram, getChildren, getSourceFile, getSourceFileChildren, isPropertySignature, isTypeAliasDeclaration, isTypeLiteralNode)
import Typescript.Utils.Enum (match)

constJust :: forall a b. a -> b -> Maybe a
constJust = Just >>> const

parseType :: ∀ e. Partial => Record (BaseNode + ()) -> Maybe (CST.Type e)
parseType n = case isTypeLiteralNode n of
  Just tln ->
    let
      parseMember :: Record (BaseNode + ()) -> Maybe (Tuple String (CST.Type e))
      parseMember memberNode = case isPropertySignature memberNode of
        Just ps ->
          let
            name = ps.name.text

            parseLiteral :: SyntaxKindEnum -> Maybe (CST.Type e)
            parseLiteral = match (Proxy :: Proxy SyntaxKindEnum) (Nothing :: Maybe (CST.Type e)) (constJust $ typeCtor "Number") (constJust $ typeCtor "String")

            tpe = ps."type" # toMaybe >>= (_.kind >>> parseLiteral)
          in
            case tpe of
              Just tl -> Just $ Tuple name tl
              Nothing -> Nothing
        Nothing -> Nothing

      members :: Array (Tuple String (CST.Type e))
      members = tln.members <#> parseMember >>= Unfoldable.fromMaybe
    in
      Just $ typeRecord members Nothing

  Nothing -> Nothing

parseDeclaration :: ∀ e. Partial => Record (BaseNode + ()) -> Maybe (CST.Declaration e)
parseDeclaration n = do
  tad <- isTypeAliasDeclaration n
  tpe <- parseType tad."type"
  pure $ declType tad.name.text [] tpe

genCode :: Array String -> Effect (Array String)
genCode fileNames = do
  program <- createProgram fileNames
  sourceFiles <- traverse (getSourceFile program) fileNames

  traverse generateOne (sourceFiles)
  where
  generateOne sf = do
    let
      generatedModule = unsafePartial $ codegenModule "Person" do -- [TODO] Get real pascal name
        let declarations = getSourceFileChildren sf >>= getChildren <#> parseDeclaration >>= Unfoldable.fromMaybe
        tell declarations
    pure $ printModule generatedModule
