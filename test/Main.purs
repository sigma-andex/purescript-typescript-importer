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
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)

main ∷ Effect Unit 
main =
  launchAff_ do
    folders ← readdir original
    specs <- discover "\\..*Spec"
    runSpec [ specReporter ] do
      (testGolden folders)
      specs
    --runSpec [ specReporter ] specs
    -- runSpec [ consoleReporter] specs
    

testGolden ∷ Array FilePath → Spec Unit
testGolden dirs = do
  describe "Golden Tests" do
    describe "formats" do
      for_ dirs $ testDir golden

testDir ∷ FilePath → FilePath → Spec Unit
testDir expected fullDirName =
  it dirName do
    fullFileNames ← readdir (original <> fullDirName)
    actuals <- liftEffect $ genCode { nodeModule: fullDirName, fileNames: (map (\f -> original <> fullDirName <> "/" <> f) fullFileNames) }
    for_ actuals \{ psFileName, psCode, esFileName, esCode } -> do

      Actual psCode `shouldBeGolden` GoldenFile (expected <> dirName <> "/" <> psFileName)
      Actual esCode `shouldBeGolden` GoldenFile (expected <> dirName <> "/" <> esFileName)
  where
  dirName = basename fullDirName

golden ∷ FilePath
golden = "testfiles/golden/"

original ∷ FilePath
original = "testfiles/original/"
