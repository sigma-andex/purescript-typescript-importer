module Person where

type Person = { name :: String, age :: Number }

foreign import getAge :: Person -> Number
