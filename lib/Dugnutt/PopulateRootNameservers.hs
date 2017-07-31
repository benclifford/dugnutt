{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}

-- | This module knows about root nameservers at compile time,
--   without needing the internet, providing the values of SBELT
--   as mentioned in RFC 1034.

module Dugnutt.PopulateRootNameservers where

  import Dugnutt.Query
  import Dugnutt.RecursiveLookup

  import Data.Void (vacuous)

  import Network.DNS (RData(..), TYPE(..))

  data PopulateRootNameservers = PopulateRootNameservers
    deriving (Show, Eq)

  instance Query PopulateRootNameservers where
    type Answer PopulateRootNameservers = ()

    launch PopulateRootNameservers = vacuous $ do 
      call $ Log "Populating pre-compiled root name server information"
      b <- call Fork
      if b
        then call $ Yield (RecursiveLookup "a.root-servers.net." A) (Right [RD_A (read "198.41.0.4")])
        else call $ Yield (RecursiveLookup "." NS) (Right [RD_NS "a.root-servers.net."])

-- Data retrieved from DNS on 2017/07/03
--  a.root-servers.net.     565044  IN      A       198.41.0.4
--  . IN NS a.root-servers.net.
