module GenCode where

import Prelude
import Control.Monad.Writer (tell)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable as Unfoldable
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync as S
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types as CST
import Tidy.Codegen (declType, printModule, typeCtor, typeRecord)
import Tidy.Codegen.Monad (codegenModule)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Typescript.Parser (BaseNode, SyntaxKindEnum, createProgram, getChildren, getSourceFile, getSourceFileChildren, isPropertySignature, isTypeAliasDeclaration, isTypeLiteralNode)
import Typescript.Utils.Enum (match)

dirs :: Effect (Array String)
dirs = S.readdir "person" <#> filter (contains (Pattern "ts")) <#> map (\n -> "person/" <> n)

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
  tad <-  isTypeAliasDeclaration n
  tpe <- parseType tad."type"
  pure $ declType tad.name.text [] tpe

genCode :: String -> Effect String
genCode fileName = do
  program <- createProgram [fileName]
  sourceFile <- getSourceFile program fileName
  let
    generatedModule =
      unsafePartial
        $ codegenModule "Person" do -- [TODO] Get real pascal name
            let
              declarations = getSourceFileChildren sourceFile >>= getChildren <#> parseDeclaration >>= Unfoldable.fromMaybe
            tell declarations
  pure $ printModule generatedModule
