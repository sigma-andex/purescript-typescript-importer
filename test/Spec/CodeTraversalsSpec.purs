module Spec.CodeTraversalsSpec
  ( expectedRewriteCode
  , spec
  ) where

import CodeTraversals
import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Writer (tell)
import Data.Array (singleton)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd)
import Debug (spy)
import Effect.Aff (Error, error)
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafePartial)
import PureScript.CST (RecoveredParserResult(..), parseType)
import PureScript.CST.Types as CST
import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)
import Tidy.Codegen (declType, printModule, typeVar)
import Tidy.Codegen.Monad (codegenModule)

testCode ∷ String
testCode =
  """
( address :: String, city :: { name :: String | r } | r )
"""

expectedRewriteCode ∷ String
expectedRewriteCode =
  """
( address :: String, city :: { name :: String | r1 } | r2 )
"""

parseTypeOrFail :: forall m. Applicative m => MonadThrow Error m => String -> m (CST.Type Void)
parseTypeOrFail code = case parseType code of
  ParseSucceeded cst -> pure cst
  -- `cst` is type `Module Void` to indicate no errors
  ParseSucceededWithErrors cst errors -> throwError (error "Failed to parse test code")
  -- `cst is type `Module RecoveredError` and contains error nodes at points of failure.
  ParseFailed err -> throwError (error "Failed to parse test code")

-- An unrecoverable error was encountered.
-- ( address :: String, city :: { name :: String | r1 } | r2 )
spec :: Spec Unit
spec =
  describe "CodeTraverls" do
    describe "typeVariables" do
      itOnly "should get all type variabls" do
        typ <- parseTypeOrFail testCode
        let
          actual = typeVariables typ <#> unwrap
          expected = [ "r", "r" ]
        actual `shouldEqual` expected
    describe "writeTypeVariables" do
      itOnly "should rewrite all type variables" do
        typ <- parseTypeOrFail testCode
        expected <- parseTypeOrFail expectedRewriteCode
        let
          fun state ident = Tuple (state + 1) (CST.Ident $ "r" <> show state)
          actual = writeTypeVariables typ 0 fun # snd
          
          -- _ = spy "actual" actual
          -- _ = spy "actual" actual
          actualCode = printModule $ unsafePartial $ codegenModule "MyTest" do
            tell $ singleton $ declType "Test" [ typeVar "r1", typeVar "r2" ] actual
          _ = spy "actualCode" actualCode
          expectedCode = printModule $ unsafePartial $ codegenModule "MyTest" do
            tell $ singleton $ declType "Test" [ typeVar "r1", typeVar "r2" ] expected
          _ = spy "expectedCode" expectedCode
        actualCode `shouldEqual` expectedCode

