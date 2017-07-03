{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}

module Dugnutt.QuerySpecificAddress where

import Dugnutt.Domain
import Dugnutt.Query

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ( (<>) )
import Network.DNS as DNS (DNSError, DNSMessage, Domain,
         FileOrNumericHost(..),
         lookupRaw,
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
  type Answer QuerySpecificAddress = Either DNSError DNSMessage

  launch q@(QuerySpecificAddress nameserver domain rrtype) = do
    call $ Log $ "QuerySpecificAddress: querying "
      ++ show nameserver ++ " for "
      ++ show domain ++ "/" ++ show rrtype
    assertNormalised domain
    let conf = ResolvConf {
            resolvInfo = RCHostName nameserver
          , resolvRetry = 10
          , resolvTimeout = 6000000
          }
    rawMsg <- liftIO $ do
      seed <- makeResolvSeed conf
      withResolver seed $ \resolver -> do
        DNS.lookupRaw resolver domain rrtype

    call $ Log $ "QuerySpecificAddress: " ++ show q ++ " => " ++ show rawMsg
    call $ Yield q rawMsg
    return ()
