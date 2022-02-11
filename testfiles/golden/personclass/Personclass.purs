module Personclass where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)

type Age = Number

foreign import getAge :: Person -> Age
foreign import createPersonImpl :: Fn2 String Age Person

createPerson :: String -> Age -> Person
createPerson name age = runFn2 createPersonImpl name age
