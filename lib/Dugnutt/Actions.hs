{-# OPTIONS_GHC -Werror -Wall #-}

-- | Helpers that are general for actions but not
--   DNS specific.
module Dugnutt.Actions where

import Dugnutt.Query

pick :: Show a => [a] -> Action a
pick [] = do
  call $ Log $ "pick: empty list"
  call End
pick [x] = do
  call $ Log $ "pick: singleton list: " ++ show x
  return x
pick (x:xs) = do
  call $ Log $ "pick: between " ++ show x ++ " and " ++ show xs
  choice <- call Fork
  if choice then return x
            else pick xs



