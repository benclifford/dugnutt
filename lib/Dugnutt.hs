{-# Language OverloadedStrings #-}

module Dugnutt where

import Dugnutt.LookupNameQuery
import Dugnutt.Query

initq :: IO ()
initq = runAction (run (LookupNameQuery "www.hawaga.org.uk"))


