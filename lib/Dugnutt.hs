{-# Language OverloadedStrings #-}

module Dugnutt where

import Dugnutt.LookupNameQuery
import Dugnutt.Query

initq :: IO ()
initq = do
  putStrLn "initq: starting query"
  let query = LookupNameQuery "www.hawaga.org.uk"
  db <- drive query
  putStrLn $ "initq: finished"
  putStrLn $ "initq: database:"
  print db
  putStrLn $ "initq: final answers to query:"
  print (findAnswers db query)
