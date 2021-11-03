module FFI.ESTree where

foreign import data ESNode :: Type

foreign import parse :: String -> ESNode

foreign import mkProgram :: Array ESNode -> ESNode

foreign import generate :: ESNode -> String