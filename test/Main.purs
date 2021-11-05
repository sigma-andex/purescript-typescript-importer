module Test.Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String.Extra (pascalCase)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import GenCode (genCode)
import Node.FS.Aff (readFile, readdir)
import Node.Path (FilePath, basename, basenameWithoutExt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions.Diff (Actual(..), GoldenFile(..), shouldBeGolden)
import Test.Spec.File (fileAsString)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main =
  launchAff_ do
    fileNames ← readdir original
    runSpec [ specReporter ] (testGolden fileNames)
    pure unit

testGolden ∷ Array FilePath → Spec Unit
testGolden fileNames = do
  describe "Golden Tests" do
    describe "formats" do
      for_ fileNames $ testOne golden

testOne ∷ FilePath → FilePath → Spec Unit
testOne expected fullFileName =
  it fileName do
    -- content ← liftEffect $ fileAsString $ original <> fileName
    actual <- liftEffect $  genCode (original <> fileName)
    Actual actual `shouldBeGolden` GoldenFile (expected <> psFileName)
  where
  fileName = basename fullFileName
  psFileName = pascalCase (basenameWithoutExt fileName "ts") <> ".purs"

golden ∷ FilePath
golden = "testfiles/golden/"

original ∷ FilePath
original = "testfiles/original/"