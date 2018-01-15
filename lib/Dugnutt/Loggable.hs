module Dugnutt.Loggable where

class Loggable m where
  logMsg :: String -> m ()

instance Loggable IO where
  logMsg = putStrLn

