{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}

module Dugnutt.QuerySpecificAddress where

import Dugnutt.Query

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ( (<>) )
import Network.DNS as DNS (DNSError, Domain, 
         FileOrNumericHost(..),
         lookup,
         makeResolvSeed,
         RData, ResolvConf(..), TYPE,
         withResolver)
import Network.Socket (HostName)

data QuerySpecificAddress = QuerySpecificAddress {
    -- | Nameserver address suitable for use in Network.DNS
    --   'FileOrNumericHost'
    nameserverAddr :: HostName

  , domain :: Domain
  , rrtype :: TYPE

  } deriving (Show, Eq)

instance Query QuerySpecificAddress where
  type Answer QuerySpecificAddress = Either DNSError [RData]

  launch q@(QuerySpecificAddress nameserver domain rrtype) = do
    call $ Log $ "QuerySpecificAddress: querying "
      ++ show nameserver ++ " for "
      ++ show domain ++ "/" ++ show rrtype
    let conf = ResolvConf {
            resolvInfo = RCHostName nameserver
          , resolvRetry = 10
          , resolvTimeout = 6000000
          }
    r <- liftIO $ do
      seed <- makeResolvSeed conf
      withResolver seed $ \resolver -> do
        DNS.lookup resolver (domain <> ".") rrtype
    call $ Log $ "QuerySpecificAddress: " ++ show q ++ " => " ++ show r
    call $ Yield q r
    return ()
