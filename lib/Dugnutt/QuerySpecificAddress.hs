{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}

module Dugnutt.QuerySpecificAddress where

import Dugnutt.Domain
import Dugnutt.Query

import Control.Monad.IO.Class (liftIO)
import Data.Void (vacuous)
import Network.DNS as DNS (DNSError, DNSMessage, Domain,
         FileOrNumericHost(..),
         lookupRaw,
         makeResolvSeed,
         ResolvConf(..), TYPE,
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

  launch q = vacuous $ do
    call $ Log $ "QuerySpecificAddress: querying "
      ++ show (nameserverAddr q) ++ " for "
      ++ show (domain q)++ "/" ++ show (rrtype q)
    assertNormalised (domain q)
    let conf = ResolvConf {
            resolvInfo = RCHostName (nameserverAddr q)
          , resolvRetry = 10
          , resolvTimeout = 6000000
          , resolvBufsize = error "resolveBufsize should not be used"
          }
    rawMsg <- liftIO $ do
      seed <- makeResolvSeed conf
      withResolver seed $ \resolver -> do
        DNS.lookupRaw resolver (domain q) (rrtype q)

    call $ Log $ "QuerySpecificAddress: " ++ show q ++ " => " ++ show rawMsg
    call $ Yield q rawMsg
