module Main where

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

constJust :: forall t20 t25. t20 -> t25 -> Maybe t20
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
            parseLiteral = match (Proxy :: Proxy SyntaxKindEnum) (Nothing :: Maybe (CST.Type e)) (constJust $ typeCtor "String") (constJust $ typeCtor "String")

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
parseDeclaration n = case isTypeAliasDeclaration n of
  Just tad -> case parseType tad."type" of
    Just tpe -> Just $ declType tad.name.text [] tpe
    Nothing -> Nothing
  Nothing -> Nothing

main :: Effect Unit
main = do
  log "---Typescript file loading---"
  files <- dirs
  log $ show files
  program <- createProgram files
  sourceFile <- getSourceFile program "person/person.ts"
  log "---Purescript codegen---"
  let
    generatedModule =
      unsafePartial
        $ codegenModule "Person" do
            let
              declarations = getSourceFileChildren sourceFile >>= getChildren <#> parseDeclaration >>= Unfoldable.fromMaybe
            tell declarations
  log $ printModule generatedModule
