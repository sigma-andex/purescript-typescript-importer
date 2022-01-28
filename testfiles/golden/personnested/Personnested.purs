module Personnested where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)

foreign import createPersonImpl :: Fn2 String (Record (HouseR r)) Person

createPerson :: String -> Record (HouseR r) -> Person
createPerson name house = runFn2 createPersonImpl name house

foreign import johnDoe :: Person
