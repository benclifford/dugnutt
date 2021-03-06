{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}

module Dugnutt.LookupNameQuery where

import Dugnutt.Query

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Network.DNS as DNS

-- the top level "lookup" function for driving the
-- lookup procedure as might be called by gethostbyname()
-- in use in a regular app.
data LookupNameQuery = LookupNameQuery {
    domain :: Domain
  } deriving (Eq, Show)

instance Query LookupNameQuery where
  type Answer LookupNameQuery = Either DNSError [RData]

  launch q = do
    ans <- liftIO $ do
      let ty = A
      seed <- makeResolvSeed defaultResolvConf
      withResolver seed $ \resolver -> do
        res <- DNS.lookup resolver (domain q) ty
        return res
    call (Log $ "LookupNameQuery: results are: " ++ show ans)
    void $ call (Yield q ans)

data FooQuery = FooQuery deriving (Eq, Show)

instance Query FooQuery where
  type Answer FooQuery = String
  launch FooQuery = do
    call (Log "FooQuery start")
    call (Log "FooQuery forking")
    b <- call Fork
    call $ Log $ "FooQuery: b = " ++ show b
    r <- call $ Launch $ LookupNameQuery $ if b then "www.hawaga.org.uk" else "www.cqx.ltd.uk"
    call $ Log $ "FooQuery/" ++ show b ++ " result: " ++ show r
    call $ Log $ "FooQuery finished"
