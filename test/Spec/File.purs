module Test.Spec.File where

import Prelude
import Data.UUID (genUUID, toString)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)

fileAsString ∷ ∀ m. MonadEffect m ⇒ String → m String
fileAsString path = liftEffect $ readTextFile UTF8 path

writeToTmpFile ∷ ∀ m. MonadEffect m ⇒ String → m String
writeToTmpFile contents = do
  fileNameUUID ← liftEffect genUUID
  writeToNamedTmpFile (toString fileNameUUID) contents

writeToNamedTmpFile ∷ ∀ m. MonadEffect m ⇒ String → String → m String
writeToNamedTmpFile fileName contents = do
  let path = "/tmp/" <> fileName <> ".html"
  liftEffect $ writeTextFile UTF8 path contents
  pure path
