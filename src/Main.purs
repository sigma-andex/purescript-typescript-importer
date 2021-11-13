module Main where

import Prelude

import Data.Array (intercalate)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Console (log)
import GenCode (genCode)

main :: Effect Unit
main = do
  log "---Typescript file loading---"
  code <- genCode [ "person/person.ts" ]
  log $ intercalate "\n" $ code <#> fst 
  log $ intercalate "\n" $ code <#> snd
