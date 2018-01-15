{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

{-# Options_GHC -Wall -Werror #-}

module Dugnutt where

import Dugnutt.Loggable
import Dugnutt.PopulateRootNameservers
import Dugnutt.Query

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, ReaderT, runReaderT)

initq :: Query q => Bool -> q -> IO ()
initq verbose query = do
  when verbose $ putStrLn "initq: starting query"

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

  -- TODO: these two drive invocations should happen in a custom
  -- environment rather
  -- than IO, so that they get custom log implementation that drive
  -- does not need to be aware of.

  let loggingEnvironment = LoggingEnvironment {
        _verbose = verbose
      }

  finalDb <- (flip runReaderT) loggingEnvironment $ do
    db <- drive [] query
    db' <- drive db PopulateRootNameservers
    return db'

  when verbose $ putStrLn $ "initq: finished"
  when verbose $ putStrLn $ "initq: final answers to query:"
  void $ mapM printLn (findAnswers finalDb query)

data LoggingEnvironment = LoggingEnvironment {
    _verbose :: Bool
  }

type ConfigurableLogger = ReaderT LoggingEnvironment IO

instance Loggable ConfigurableLogger where
  logMsg s = do
    config <- ask
    when (_verbose config) $ liftIO $ putStrLn s


printLn :: Show v => v -> IO ()
printLn = putStrLn . show
