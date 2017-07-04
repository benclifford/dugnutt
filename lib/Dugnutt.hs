{-# Language OverloadedStrings #-}

module Dugnutt where

import Dugnutt.LookupNameQuery
import Dugnutt.PopulateRootNameservers
import Dugnutt.RecursiveLookup
import Dugnutt.Query

import Control.Monad (mapM_)
import Network.DNS

initq :: Query q => q -> IO ()
initq query = do
  putStrLn "initq: starting query"

  -- We can meaningfully drive multiple queries in a row
  -- sharing the db between them: sharing the db means
  -- accumulating the db across drive runs: anything more
  -- that needs to run from the first query due to information
  -- gathered in the second query will happen in that second
  -- drive, because the DB contains callbacks.
  -- so drives-of-queries form a monoid.
  -- (and so queries can form a monoid, that is related to
  -- `mplus` of Actions, somehow - perhaps:
  --   (Launch q1 >> End) `mplus` (Launch q2 >> End))
  --   is 'the same as' driving (q1 <> q2)
  -- where 'the same as' means something like "the db contains
  -- the same answers at the end".

  -- this ability to drive sequentially is related to the
  -- decomposition of drive into a series of cleanly separated
  -- stages which are related by a work queue and a database:
  -- rather than a work queue, we have separate invocations of
  -- drive, but the database is passed on in the same fashion.

  db <- drive [] query
  db' <- drive db PopulateRootNameservers
  putStrLn $ "initq: finished"
  -- putStrLn $ "initq: database:"
  -- mapM_ printLn db'
  putStrLn $ "initq: final answers to query:"
  print (findAnswers db' query)

printLn :: Show v => v -> IO ()
printLn = putStrLn . show
