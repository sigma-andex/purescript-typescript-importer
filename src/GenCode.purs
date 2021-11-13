module GenCode where

import Prelude

import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (toMaybe)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable as Unfoldable
import Debug (spy)
import Effect (Effect)
import FFI.ESTree as ES
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types as CST
import Tidy.Codegen (declForeign, declType, printModule, typeArrow, typeCtor, typeRecord)
import Tidy.Codegen.Monad (codegenModule)
import Type.Row (type (+))
import Typescript.Parser as TS
import Typescript.SyntaxKind as SK
import Typescript.Utils.Enum (default', on')

constJust :: forall a b. a -> b -> Maybe a
constJust = Just >>> const

newtype ModuleName = ModuleName String
instance Newtype ModuleName String

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
      toMembers tnInner = tnInner <#> parseMember >>= Unfoldable.fromMaybe

    parseTypeReference :: { | TS.TypeNodeR + n } -> Maybe (CST.Type e)
    parseTypeReference tn = TS.isTypeReferenceNode tn <#> \ref -> typeCtor ref.typeName.text

    caseFn :: SK.SyntaxKindEnum -> Maybe (CST.Type e)
    caseFn =
      default' (Nothing :: Maybe (CST.Type e))
        # on' SK.typeLiteral (const (parseTypeLiteralNode n))
        # on' SK.typeReference (const (parseTypeReference n))
        # on' SK.numberKeyword (constJust $ typeCtor "Number")
        # on' SK.stringKeyword (constJust $ typeCtor "String")
  in
    caseFn kind

parseNode :: ∀ n e. Partial => ModuleName -> { | TS.NodeR + n } -> Maybe (CST.Declaration e ) /\ Maybe (ES.ESNode)
parseNode (ModuleName moduleName) n = case TS.isTypeAliasDeclaration n of
  Just tad -> 
    let
      psDeclaration = do
        tpe <- parseTypeNode tad."type"
        pure $ declType tad.name.text [] tpe 
    in psDeclaration /\ Nothing
  Nothing -> 
    let
      psDeclaration = do
        fd <- TS.isFunctionDeclaration n
        name <- toMaybe fd.name
        tpe <- toMaybe fd."type" >>= parseTypeNode
        let
          params :: Array (Maybe TS.TypeNode)
          params = fd.parameters <#> (_."type" >>> toMaybe)
        members <- sequence $ params <#> (\mp -> mp >>= parseTypeNode)
        let
          fn = declForeign name.text
            ( typeArrow
                members
                tpe
            )
        Just { ps: fn, name: name.text } 
      
      jsDeclaration = psDeclaration <#> \{ name } -> ES.parse $ "exports." <> name <> " = " <> moduleName <> "." <> name <> ";"

    in (psDeclaration <#> _.ps) /\ jsDeclaration

genCode :: Array String -> Effect (Array (String /\ String))
genCode fileNames = do
  program <- TS.createProgram fileNames
  sourceFiles <- traverse (\fn -> TS.getSourceFile program fn <#> \sf -> Tuple fn sf) fileNames

  traverse generateOne (sourceFiles)
  where
  generateOne (Tuple fn sf) = unsafePartial $ do
    let
      _ = spy "Filename" fn
      moduleName = "Person"
      nodeModuleName = "person"
      
      declarations = TS.getSourceFileChildren sf >>= TS.getChildren <#> parseNode (ModuleName moduleName)
      psDeclarations = declarations <#> fst >>= Unfoldable.fromMaybe
      jsDeclarations = declarations <#> snd >>= Unfoldable.fromMaybe

      generatedPsModule = codegenModule "Person" do -- [TODO] Get real pascal name
        tell psDeclarations
      generatedJsModule = ES.mkProgram $ [
        ES.parse "\"use strict\";",
        ES.parse $ "const " <> moduleName <> " = require(\"" <> nodeModuleName <> "\")"
      ] <> jsDeclarations
    pure $ printModule generatedPsModule /\ (ES.generate generatedJsModule)
