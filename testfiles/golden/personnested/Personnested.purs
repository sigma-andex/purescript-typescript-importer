module Personnested where

import Data.Function.Uncurried (Fn2, runFn2)

type HouseR r = (address :: String | r)
type House = Record (HouseR ())

type PersonR r r2 = (name :: String, house :: Record (HouseR r2) | r)
type Person = Record (PersonR () ())

foreign import createPersonImpl :: forall r. Fn2 String (Record (HouseR r)) Person

createPerson :: forall r. String -> Record (HouseR r) -> Person
createPerson name house = runFn2 createPersonImpl name house

foreign import johnDoe :: Person
