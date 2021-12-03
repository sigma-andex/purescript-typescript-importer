module GenCode
  ( ModuleName(..)
  , Parameter
  , constJust
  , genCode
  , parseFunctionDeclaration
  , parseNode
  , parseTypeAliasDeclaration
  , parseVariableDeclaration
  , parseVariableStatement
  ) where

import Prelude

import Control.Monad.Writer (tell)
import Data.Array (foldl, length, singleton)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe)
import Data.String (trim)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable as Unfoldable
import Debug (spy)
import Effect (Effect)
import FFI.ESTree as ES
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types as CST
import Tidy.Codegen (binderVar, declForeign, declSignature, declType, declValue, exprApp, exprIdent, printModule, typeApp, typeArrow, typeCtor, typeRecord)
import Tidy.Codegen.Monad (CodegenT, codegenModule, importFrom, importType, importValue)
import Type.Row (type (+))
import Typescript.Parser as TS
import Typescript.SyntaxKind as SK
import Typescript.Utils.Enum (default', on')

constJust :: forall a b. a -> b -> Maybe a
constJust = Just >>> const

newtype ModuleName = ModuleName String

instance Newtype ModuleName String

mkNullable :: forall e. Partial => CST.Type e -> CST.Type e
mkNullable t = typeApp (typeCtor "Nullable") [ t ]

parseTypeNode :: ∀ n e. Partial => { | TS.TypeNodeR + n } -> Maybe (CST.Type e)
parseTypeNode n@{ kind } =
  let
    parseMember :: TS.TypeNode -> Maybe (Tuple String (CST.Type e))
    parseMember memberNode = do
      ps <- TS.isPropertySignature memberNode
      let
        name = ps.name.text
        isNullable = isJust $ toMaybe ps.questionToken
        tpe = ps."type" # toMaybe >>= parseTypeNode
      tl <- tpe
      Just $ Tuple name if isNullable then mkNullable tl else tl

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

parseTypeAliasDeclaration :: ∀ e m. Monad m => Partial => ModuleName -> CodegenT e m Unit -> TS.TypeAliasDeclaration -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
parseTypeAliasDeclaration (ModuleName moduleName) codegen tad = case parseTypeNode tad."type" of
  Just tpe -> { es: [], ps: codegen >>= const (tell [ declType tad.name.text [] tpe ]) }
  Nothing -> { es: [], ps: codegen }

type Parameter e =
  { name :: String
  , isNullable :: Boolean
  , tpe :: CST.Type e
  }

parseFunctionDeclaration :: ∀ e m. Monad m => Partial => ModuleName -> CodegenT e m Unit -> TS.FunctionDeclaration -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
parseFunctionDeclaration (ModuleName moduleName) codegen fd =
  let
    maybeFnInfo = do
      name <- toMaybe fd.name
      tpe <- toMaybe fd."type" >>= parseTypeNode
      let

        parseParam :: TS.ParameterDeclaration -> Maybe (Parameter e)
        parseParam { questionToken, "type": paramTpe, name } =
          toMaybe paramTpe >>= parseTypeNode <#> { isNullable: isJust $ toMaybe questionToken, name: name.text, tpe: _ }

        params :: Array (Maybe (Parameter e))
        params = fd.parameters <#> parseParam

        nullableMembers :: Array (Parameter e)
        nullableMembers = params >>= Unfoldable.fromMaybe
      pure { name, nullableMembers, tpe }

    result = case maybeFnInfo of
      Just { name, nullableMembers, tpe } ->
        let
          handleNullables nm = nm <#> \{ isNullable, tpe: t } -> if isNullable then mkNullable t else t
        in
          case length nullableMembers of
            0 -> { es: [], ps: codegen } -- [TODO]: deal with effects
            1 ->
              let
                es = singleton $ ES.parse $ "exports." <> name.text <> " = " <> moduleName <> "." <> name.text <> ";"
                ps = do
                  nullableTpe <- importFrom "Data.Nullable" (importType "Nullable")
                  tell $ singleton $ declForeign name.text (typeArrow (handleNullables nullableMembers) tpe)
              in
                { es, ps: codegen >>= const ps }
            paramNumber | paramNumber > 0 && paramNumber <= 10 ->
              let
                es = singleton $ ES.parse $ "exports." <> (name.text <> "Impl") <> " = " <> moduleName <> "." <> name.text <> ";"
                ps = do
                  uncurriedTpe <- importFrom "Data.Function.Uncurried" (importType $ "Fn" <> show paramNumber)
                  runFn <- importFrom "Data.Function.Uncurried" (importValue $ "runFn" <> show paramNumber)
                  maybeTpe <- importFrom "Data.Maybe" (importType "Maybe")
                  nullableTpe <- importFrom "Data.Nullable" (importType "Nullable")
                  toNullableFn <- importFrom "Data.Nullable" (importValue "toNullable")
                  let
                    members = handleNullables nullableMembers
                    uncurriedName = (name.text <> "Impl")
                    uncurriedFn = declForeign uncurriedName
                      ( typeApp (typeCtor uncurriedTpe)
                          (members <> [ tpe ])
                      )
                    nameBinders = nullableMembers <#> (binderVar <<< _.name)
                    nameExprs = nullableMembers <#> \{ name: n, isNullable } ->
                      if isNullable then exprApp (exprIdent toNullableFn) [ exprIdent n ]
                      else exprIdent n
                    maybeMembers = nullableMembers <#> \{ isNullable, tpe: t } ->
                      if isNullable then typeApp (typeCtor maybeTpe) [ t ]
                      else t
                  tell
                    [ uncurriedFn
                    , declSignature name.text (typeArrow maybeMembers tpe)
                    , declValue name.text nameBinders (exprApp (exprIdent runFn) $ [ exprIdent uncurriedName ] <> nameExprs)
                    ]
              in
                { es, ps: codegen >>= const ps }
            _ -> { es: [], ps: codegen }
      Nothing -> { es: [], ps: codegen }
  in
    result

parseVariableDeclaration :: ∀ e m. Monad m => Partial => ModuleName -> CodegenT e m Unit -> TS.VariableDeclaration -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
parseVariableDeclaration (ModuleName moduleName) codegen { name, "type": tpe } = case toMaybe tpe >>= parseTypeNode of
  Just tpeNode ->
    let
      es = singleton $ ES.parse $ "exports." <> (name.text) <> " = " <> moduleName <> "." <> name.text <> ";"
      ps = tell $ singleton $ declForeign name.text tpeNode
    in
      { es, ps: codegen >>= const ps }
  Nothing ->
    -- [TODO]: decide what we do if we don't have a type annotation
    { es: [], ps: codegen }

parseVariableStatement :: ∀ e m. Monad m => Partial => ModuleName -> CodegenT e m Unit -> TS.VariableStatement -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
parseVariableStatement mn codegen vs =
  let
    acc { es: esInput, ps: psInput } vd =
      let
        { es: esOutput, ps: psOutput } = parseVariableDeclaration mn psInput vd
      in
        { es: esInput <> esOutput, ps: psOutput }
    result = foldl acc { es: [], ps: codegen } vs.declarationList.declarations
  in
    result

parseNode :: ∀ n e m. Monad m => Partial => ModuleName -> CodegenT e m Unit -> { | TS.NodeR + n } -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
parseNode moduleName codegen n@{ kind } =
  let
    empty = { es: [], ps: codegen }

    getOrEmpty = maybe empty identity

    mkHandler
      :: forall t enum
       . ({ | TS.NodeR + n } -> Maybe t)
      -> (ModuleName -> CodegenT e m Unit -> t -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit })
      -> enum
      -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
    mkHandler isOfType parseType = const $ isOfType n <#> parseType moduleName codegen # getOrEmpty

    caseFn :: SK.SyntaxKindEnum -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
    caseFn =
      default' empty
        # on' SK.typeAliasDeclaration (mkHandler TS.isTypeAliasDeclaration parseTypeAliasDeclaration)
        # on' SK.functionDeclaration (mkHandler TS.isFunctionDeclaration parseFunctionDeclaration)
        # on' SK.variableStatement (mkHandler TS.isVariableStatement parseVariableStatement)
  in
    caseFn kind

genCode :: Array String -> Effect (Array (String /\ String))
genCode fileNames = do
  program <- TS.createProgram fileNames
  sourceFiles <- traverse (\fn -> TS.getSourceFile program fn <#> \sf -> Tuple fn sf) fileNames
  traverse generateOne sourceFiles
  where
  generateOne (Tuple fn sf) = unsafePartial $ do
    let
      _ = spy "Filename" fn
      moduleName = "Person"
      nodeModuleName = "person"

      acc { es: esInput, ps: psInput } node =
        let
          { es: esOutput, ps: psOutput } = parseNode (ModuleName moduleName) psInput node
        in
          { es: esInput <> esOutput, ps: psOutput }

      declarations = TS.getSourceFileChildren sf >>= TS.getChildren # foldl acc { es: [], ps: pure unit }

      generatedPsModule = codegenModule "Person" declarations.ps
      generatedJsModule = ES.mkProgram $
        [ ES.parse "'use strict';"
        , ES.parse $ "const " <> moduleName <> " = require('" <> nodeModuleName <> "')"
        ] <> (declarations.es)
    pure $ printModule generatedPsModule /\ (ES.generate generatedJsModule # trim # (_ <> "\n"))
