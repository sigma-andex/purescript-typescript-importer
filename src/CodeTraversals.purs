module CodeTraversals where

import Data.Lens.Barlow
import Prelude
import Prim hiding (Row, Type)

import Control.Monad.Writer (tell)
import Data.Argonaut (stringify)
import Data.Array (foldl, length, singleton)
import Data.Array as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Bifunctor (rmap)
import Data.Bitraversable (rtraverse)
import Data.Foldable (intercalate, null)
import Data.Lens.Traversal (traverseOf)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Nullable as Nullable
import Data.String (trim)
import Data.String.Extra as SE
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable as Unfoldable
import Debug (spy)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Unsafe (unsafePerformEffect)
import FFI.ESTree as ES
import Node.Encoding (Encoding(..))
import Node.FS.Aff as AFS
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import Prim as P
import PureScript.CST.Traversal as Traversal
import PureScript.CST.Types (Wrapped)
import PureScript.CST.Types as CST
import Tidy.Codegen (binderVar, declForeign, declSignature, declType, declValue, exprApp, exprIdent, printModule, typeApp, typeArrow, typeCtor, typeRecord, typeRow, typeRowEmpty, typeVar)
import Tidy.Codegen.Monad (CodegenT, codegenModule, importFrom, importType, importValue)
import Type.Row (type (+))
import Typescript.Parser (TypeChecker)
import Typescript.Parser as TS
import Typescript.SyntaxKind as SK
import Typescript.Utils.Enum (default', on')
import Unsafe.Coerce (unsafeCoerce)

typeVariables :: forall e. CST.Type e -> Array (CST.Ident)
typeVariables = case _ of 
  (CST.TypeVar n) -> [(unwrap n).name]
  (CST.TypeConstructor qn) -> []
  (CST.TypeWildcard st) -> []
  (CST.TypeHole id) -> []
  (CST.TypeString st s) -> []
  (CST.TypeRow row) -> typeRowVariables $ (unwrap row).value   -- ( s)
  (CST.TypeRecord record) -> typeRowVariables $ (unwrap record).value -- { name :: }
  (CST.TypeForall st1 bindings st2 t) -> [] 
  (CST.TypeKinded t1 st t2) -> []
  (CST.TypeApp t1 t2) -> []
  (CST.TypeOp t1 ops) -> []
  (CST.TypeOpName op) -> []
  (CST.TypeArrow t1 st t2 ) -> []
  (CST.TypeArrowName st) -> []
  (CST.TypeConstrained t1 st t2) -> []
  (CST.TypeParens t) -> []
  (CST.TypeUnaryRow st t) -> []
  (CST.TypeError e) -> []
  
-- ( address :: String, city :: { name :: String | r } | r )

-- forall r1 r2. ( address :: String, city :: { name :: String | r1 } | r2 )

  
writeTypeVariables :: forall e m. Monad m => (CST.Ident -> m CST.Ident) -> CST.Type e -> m (CST.Type e)
writeTypeVariables fun typ = case typ of 
  (CST.TypeVar n) -> do
      ident <- fun (unwrap n).name
      pure $ typeVar ident
  (CST.TypeRow tr) ->
    CST.TypeRow <$> traverseOf (barlow (key :: _ "!.value")) (writeTypeRowTypeVariables fun) tr
    -- let
    --   newState = writeTypeRowTypeVariables (Barlow.view (key :: _ "!.value") tr) state fun
    --   result = rmap (\newValue -> CST.TypeRow $ Barlow.set (key :: _ "!.value") newValue tr) newState
    -- in
    --   result
  otherwise -> pure otherwise

writeTypeRowTypeVariables :: forall m e. Monad m => (CST.Ident -> m CST.Ident) -> CST.Row e -> m (CST.Row e)
writeTypeRowTypeVariables fun (CST.Row { labels: Just (CST.Separated { head: CST.Labeled { label, value, separator }, tail: separatedTail }), tail }) = 
    -- Tuple state $ typeRow [] Nothing
    -- let
      -- Tuple newState1 newIdent = writeTypeVariables value state fun
      -- Tuple newState2 newSeparatedTail = foldl (\(Tuple ns nt) t -> nt <> [writeTypeVariables t ns fun]) newState1 separatedTail
      -- Tuple newState3 newTail = foldl (\(Tuple ns nt) t -> nt <> [writeTypeVariables t state fun]) newState2 tail
    -- in
      pure (CST.Row { labels: Just (CST.Separated { head: CST.Labeled { label, value: value, separator }, tail: separatedTail }), tail })
  -- let 
  --   _ = spy "r" r 
  -- in Array.foldl (\acc (_ /\ CST.Labeled { value: elem } ) -> acc <> (typeVariables elem) ) (typeVariables value) separatedTail <> 
  --   (maybe [] (snd >>> typeVariables) tail)
writeTypeRowTypeVariables fun o = pure o

typeRowVariables :: forall e. CST.Row e -> Array (CST.Ident)
typeRowVariables r@(CST.Row { labels: Just (CST.Separated { head: CST.Labeled { label, value }, tail: separatedTail }), tail }) = 
 Array.foldl (\acc (_ /\ CST.Labeled { value: elem } ) -> acc <> (typeVariables elem) ) (typeVariables value) separatedTail <> 
    (maybe [] (snd >>> typeVariables) tail)
typeRowVariables _ = []
