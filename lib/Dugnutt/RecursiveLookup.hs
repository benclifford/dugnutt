{-# Language TypeFamilies #-}
module Dugnutt.RecursiveLookup where

import Data.ByteString.Char8 (unpack, pack)
import Data.IP
import Data.List (sort, tails, intersperse)
import Data.List.Split (splitOn)

import Network.DNS

import Dugnutt.Query

import Dugnutt.LookupNameQuery
import Dugnutt.QuerySpecificAddress

data RecursiveLookup = RecursiveLookup {
    domain :: Domain
  , rrtype :: TYPE
  } deriving (Eq, Show)

instance Query RecursiveLookup where
  type Answer RecursiveLookup = Either DNSError [RData]

  launch q@(RecursiveLookup domain rrtype) = do
    call $ Log $ "RecursiveLookup(" ++ unpack domain ++ "/" ++ show rrtype ++ ") start"

    -- TODO: simulate cache checking by asking for
    -- nameservers for all zone components up to the
    -- root.

    possibleAncestorZone <- splitByZone domain

    nameserverHostname <- getNameserverForZone possibleAncestorZone

    nameserverAddress <- getAddressForHost nameserverHostname

    -- TODO: do a directed query to the specified nameserver address,
    -- for the domain/rrtype, giving an rrset of results (or perhaps
    -- an Either err [RData])
    res <- call $ Launch (QuerySpecificAddress (show nameserverAddress) domain rrtype)

    -- so we can yield the whole rrset/error now - this should not be
    -- unpacked into individual records because sometimes we should
    -- reason about the set as a whole.

    let resSorted = fmap sort res
    call $ Log $ "RecursiveLookup(" ++ unpack domain ++ "/" ++ show rrtype ++ ") => " ++ show resSorted
    call $ Yield q resSorted
    return () -- never reached because of Yield

splitByZone :: Domain -> Action Domain
splitByZone domain = do
  call $ Log $ "splitByZone: splitting zone: " ++ show domain
  let domainAsString = unpack domain

  let parts = splitOn "." domainAsString
 
  call $ Log $ "splitByZone: parts: " ++ show parts

  let ts = tails parts

  call $ Log $ "splitByZone: tails: " ++ show ts

  -- non-deterministically continue with one (all) of the
  -- possible tails.
  t <- pick ts
  call $ Log $ "splitByZone: picked tail: " ++ show t

  let reassembled = concat $ intersperse "." t

  let packed = pack reassembled

  call $ Log $ "splitByZone: ancestor: " ++ show packed

  return packed

getNameserverForZone :: Domain -> Action Domain
getNameserverForZone zoneName = do
  call $ Log $ "getNameserverForZone: zone " ++ show zoneName
  (Right ns_rrset) <- call $ Launch $ RecursiveLookup zoneName NS
  call $ Log $ "getNameserverForZone: zone " ++ show zoneName
            ++ " has NS RRSet " ++ show ns_rrset
  -- this might fail, rather than return a nameserver name
  -- so maybe we need to deal with that error being returned.
  -- perhaps we should actually return error here, and then
  -- turn that into more generally the resolution failing?
  -- (but not killing dugnutt...)
  (RD_NS ns_name) <- pick ns_rrset

  call $ Log $ "getNameserverForZone: zone " ++ show zoneName
            ++ " has nameserver " ++ show ns_name 
  return ns_name

-- TODO: returns (non-deterministically) the A/AAAA records for the given zone
getAddressForHost :: Domain -> Action IPv4
getAddressForHost hostName = do
  call $ Log $ "getAddressForHost: looking up address for host " 
            ++ show hostName
  (Right a_rrset) <- call $ Launch $ RecursiveLookup hostName A
  call $ Log $ "getAddressForHost: host " ++ show hostName
            ++ " has A RRSet " ++ show a_rrset
  (RD_A a) <- pick a_rrset

  call $ Log $ "getAddressForHost: host " ++ show hostName
            ++ " has address " ++ show a
  return a

pick :: Show a => [a] -> Action a
pick [] = do
  call $ Log $ "pick: empty list"
  call End
pick [x] = do
  call $ Log $ "pick: singleton list: " ++ show x
  return x
pick (x:xs) = do
  call $ Log $ "pick: between " ++ show x ++ " and " ++ show xs
  choice <- call Fork
  if choice then return x
            else pick xs

