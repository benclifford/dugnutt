{-# Language TypeFamilies #-}

module Dugnutt.LookupNameQuery where

import Dugnutt.Query

import Control.Monad.IO.Class (liftIO)
import Network.DNS as DNS

-- the top level "lookup" function for driving the
-- lookup procedure as might be called by gethostbyname()
-- in use in a regular app.
data LookupNameQuery = LookupNameQuery {
    domain :: Domain
  }

instance Query LookupNameQuery where
  type Answer LookupNameQuery = Either DNSError [RData]

  run q@(LookupNameQuery domain) = do
    ans <- liftIO $ do
      let ty = A
      seed <- makeResolvSeed defaultResolvConf
      withResolver seed $ \resolver -> do
        res <- DNS.lookup resolver domain ty
        putStr "results are: "
        print res
        return res
    call (Yield q ans)
    return ()
