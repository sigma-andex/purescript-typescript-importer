module Test.Main where

import Prelude

import Data.Array (zip)
import Data.Foldable (for_)
import Data.String.Extra (pascalCase)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import GenCode (genCode)
import Node.FS.Aff (readdir)
import Node.Path (FilePath, basename, basenameWithoutExt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions.Diff (Actual(..), GoldenFile(..), shouldBeGolden)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main =
  launchAff_ do
    folders ← readdir original
    runSpec [ specReporter ] (testGolden folders)
    pure unit

testGolden ∷ Array FilePath → Spec Unit
testGolden dirs = do
  describe "Golden Tests" do
    describe "formats" do
      for_ dirs $ testDir golden

testDir ∷ FilePath → FilePath → Spec Unit
testDir expected fullDirName =
  it dirName do
    fullFileNames ← readdir (original <> fullDirName)
    actuals <- liftEffect $ genCode (map (\f -> original <> fullDirName <> "/" <> f) fullFileNames)
    for_ (zip fullFileNames actuals) \(Tuple fileName actual) -> do
      let 
        goldenFileName = pascalCase (basenameWithoutExt fileName "ts")
        psFileName = goldenFileName <> ".purs"
        jsFileName = goldenFileName <> ".js"

      Actual (fst actual) `shouldBeGolden` GoldenFile (expected <> dirName <> "/" <> psFileName)
      Actual (snd actual) `shouldBeGolden` GoldenFile (expected <> dirName <> "/" <> jsFileName)
  where
  dirName = basename fullDirName

golden ∷ FilePath
golden = "testfiles/golden/"

original ∷ FilePath
original = "testfiles/original/"
