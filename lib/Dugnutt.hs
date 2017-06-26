{-# Language OverloadedStrings #-}

module Dugnutt where

import Dugnutt.LookupNameQuery
import Dugnutt.Query

initq :: IO ()
initq = do
  putStrLn "dugnutt: initq: starting query"
  res <- drive (LookupNameQuery "www.hawaga.org.uk")
  putStrLn $ "dugnutt: initq: finished"
  putStrLn $ "Database:"
  print res

