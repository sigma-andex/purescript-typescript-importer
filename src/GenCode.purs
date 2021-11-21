module GenCode where

import Prelude

import Control.Monad.Writer (tell)
import Data.Array (foldl, length, singleton)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (toMaybe)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable as Unfoldable
import Debug (spy)
import Effect (Effect)
import FFI.ESTree as ES
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types as CST
import Tidy.Codegen (declForeign, declSignature, declType, declValue, exprApp, exprIdent, printModule, typeApp, typeArrow, typeCtor, typeRecord)
import Tidy.Codegen.Monad (CodegenT, codegenModule, importFrom, importType, importValue)
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

parseTypeAliasDeclaration :: ∀ e m. Monad m => Partial => ModuleName -> CodegenT e m Unit -> TS.TypeAliasDeclaration -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
parseTypeAliasDeclaration (ModuleName moduleName) codegen tad = case parseTypeNode tad."type" of
  Just tpe -> { es: [], ps: codegen >>= const (tell [ declType tad.name.text [] tpe ]) }
  Nothing -> { es: [], ps: codegen }

parseFunctionDeclaration :: ∀ e m. Monad m => Partial => ModuleName -> CodegenT e m Unit -> TS.FunctionDeclaration -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
parseFunctionDeclaration (ModuleName moduleName) codegen fd =
  let
    maybeFnInfo = do
      name <- toMaybe fd.name
      tpe <- toMaybe fd."type" >>= parseTypeNode
      let
        params :: Array (Maybe TS.TypeNode)
        params = fd.parameters <#> (_."type" >>> toMaybe)
      members <- sequence $ params <#> (\mp -> mp >>= parseTypeNode)
      pure { name, members, tpe }

    result = case maybeFnInfo of
      Just { name, members, tpe } ->
        case length members of
          0 -> { es: [], ps: codegen } -- [TODO]: deal with effects
          1 ->
            let
              es = singleton $ ES.parse $ "exports." <> name.text <> " = " <> moduleName <> "." <> name.text <> ";"
              ps = tell $ singleton $ declForeign name.text (typeArrow members tpe)
            in
              { es, ps: codegen >>= const ps }
          paramNumber | paramNumber > 0 && paramNumber <= 10 ->
            let
              es = singleton $ ES.parse $ "exports." <> (name.text <> "Impl") <> " = " <> moduleName <> "." <> name.text <> ";"
              ps = do
                uncurriedTpe <- importFrom "Data.Function.Uncurried" (importType $ "Fn" <> show paramNumber)
                runFn <- importFrom "Data.Function.Uncurried" (importValue $ "runFn" <> show paramNumber)
                let
                  uncurriedName = (name.text <> "Impl")
                  uncurriedFn = declForeign uncurriedName
                    ( typeApp (typeCtor uncurriedTpe)
                        (members <> [ tpe ])
                    )
                tell
                  [ uncurriedFn
                  , declSignature name.text (typeArrow members tpe)
                  , declValue name.text [] (exprApp (exprIdent runFn) [ exprIdent uncurriedName ])
                  ]
            in
              { es, ps: codegen >>= const ps }
          _ -> { es: [], ps: codegen }
      Nothing -> { es: [], ps: codegen }
  in
    result

parseNode :: ∀ n e m. Monad m => Partial => ModuleName -> CodegenT e m Unit -> { | TS.NodeR + n } -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
parseNode moduleName codegen n@{ kind } =
  let
    empty = { es: [], ps: codegen }

    getOrEmpty = maybe empty identity

    handleTypeAliasDeclaration = TS.isTypeAliasDeclaration n <#> parseTypeAliasDeclaration moduleName codegen # getOrEmpty
    handleFunctionDeclaration = TS.isFunctionDeclaration n <#> parseFunctionDeclaration moduleName codegen # getOrEmpty

    caseFn :: SK.SyntaxKindEnum -> { es :: Array ES.ESNode, ps :: CodegenT e m Unit }
    caseFn =
      default' empty
        # on' SK.typeAliasDeclaration (const handleTypeAliasDeclaration)
        # on' SK.functionDeclaration (const handleFunctionDeclaration)
  in
    caseFn kind

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

      acc { es: esInput, ps: psInput } node =
        let
          { es: esOutput, ps: psOutput } = parseNode (ModuleName moduleName) psInput node
        in
          { es: esInput <> esOutput, ps: psOutput }

      declarations = TS.getSourceFileChildren sf >>= TS.getChildren # foldl acc { es: [], ps: pure unit }

      generatedPsModule = codegenModule "Person" declarations.ps
      generatedJsModule = ES.mkProgram $
        [ ES.parse "\"use strict\";"
        , ES.parse $ "const " <> moduleName <> " = require(\"" <> nodeModuleName <> "\")"
        ] <> (declarations.es)
    pure $ printModule generatedPsModule /\ (ES.generate generatedJsModule)
