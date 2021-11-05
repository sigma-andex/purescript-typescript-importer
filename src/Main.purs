module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import GenCode(genCode)


main :: Effect Unit
main = do
  log "---Typescript file loading---"
  code <- genCode "person/person.ts"
  log $ code
