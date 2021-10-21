module PersonP where

type Person = { name :: String, age :: Int }

foreign import getAge :: Person -> Number