module Personnamespaced where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)

type Person = { name :: String, age :: Number }

foreign import getAge :: Person -> Number
foreign import createPersonImpl :: Fn2 String Number Person

createPerson :: String -> Number -> Person
createPerson name age = runFn2 createPersonImpl name age

foreign import johnDoe :: Person
foreign import createPersonOrDefaultImpl :: Fn2 (Nullable String) (Nullable Number) Person

createPersonOrDefault :: Maybe String -> Maybe Number -> Person
createPersonOrDefault name age = runFn2 createPersonOrDefaultImpl (toNullable name)
  (toNullable age)

foreign import createPersonWithNameImpl :: Fn2 String (Nullable Number) Person

createPersonWithName :: String -> Maybe Number -> Person
createPersonWithName name age = runFn2 createPersonWithNameImpl name (toNullable age)

type NullablePerson = { name :: Nullable String, age :: Nullable Number }
