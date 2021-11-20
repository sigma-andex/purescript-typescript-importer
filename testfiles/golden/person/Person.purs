module Person where

import Data.Function.Uncurried (Fn2, runFn2)

type Person = { name :: String, age :: Number }

foreign import getAge :: Person -> Number
foreign import createPersonImpl :: Fn2 String Number Person

createPerson :: String -> Number -> Person
createPerson = runFn2 createPersonImpl
