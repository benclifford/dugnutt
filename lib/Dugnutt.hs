{-# Language OverloadedStrings #-}

module Dugnutt where

import Dugnutt.LookupNameQuery
import Dugnutt.Query

initq :: IO ()
initq = do
  putStrLn "initq: starting query"
  res <- drive (LookupNameQuery "www.hawaga.org.uk")
  putStrLn $ "initq: finished"
  putStrLn $ "initq: database:"
  print res

