{-# Language TypeFamilies #-}
module Dugnutt.RecursiveLookup where

import Control.Monad (mplus)
import Data.ByteString.Char8 (unpack, pack)
import Data.Monoid ( (<>) )
import Data.IP
import Data.List (groupBy, sort, sortBy, tails, intersperse)
import Data.List.Split (splitOn)

import qualified Network.DNS as DNS

import Dugnutt.Domain
import Dugnutt.Query

import qualified Dugnutt.QuerySpecificAddress as QSA

data RecursiveLookup = RecursiveLookup {
    domain :: DNS.Domain
  , rrtype :: DNS.TYPE
  } deriving (Eq, Show)

instance Query RecursiveLookup where
  type Answer RecursiveLookup = Either DNS.DNSError [DNS.RData]

  launch q = do
    call $ Log $ "RecursiveLookup(" ++ unpack (domain q) ++ "/" ++ show (rrtype q) ++ ") start"

    assertNormalised (domain q)

    possibleAncestorZone <- splitByZone (domain q)

    assertNormalised possibleAncestorZone

    call $ Log $ "RecursiveLookup(" ++ unpack (domain q) ++ "/" ++ show (rrtype q) ++ ") possible ancester zone: " ++ show possibleAncestorZone
    nameserverHostname <- getNameserverForZone possibleAncestorZone
    call $ Log $ "RecursiveLookup(" ++ unpack (domain q) ++ "/" ++ show (rrtype q) ++ ") possible ancester zone: " ++ show possibleAncestorZone ++ "  nameserver: " ++ show nameserverHostname

    assertNormalised nameserverHostname

    nameserverAddress <- getAddressForHost nameserverHostname
    call $ Log $ "RecursiveLookup(" ++ unpack (domain q) ++ "/" ++ show (rrtype q) ++ ") possible ancester zone: " ++ show possibleAncestorZone ++ "  nameserver: " ++ show nameserverHostname ++ "  nameserverAddress " ++ show nameserverAddress

    res <- call $ Launch (QSA.QuerySpecificAddress (show nameserverAddress) (domain q) (rrtype q))

    -- res is a raw message, which needs some processing. The Network.DNS lookup
    -- function takes only the answer section, and only records of the correct type
    -- (so only exactly the answer - what happens for CNAMEs in Network.DNS?)

    -- in addition to partitioning the results, we need to ensure that
    -- we yield a query result for the original query, even if the result
    -- set is empty, I think - when we didn't get an error from the lookup.
    -- so as to preserve the fact that we did get some kind of answer of that
    -- specific query.
    case res of
      Right rawMsg -> yieldRawMessage q rawMsg
      Left err -> (call $ Yield q (Left err)) >> return ()
    return () -- never reached because of Yield


yieldRawMessage q@(RecursiveLookup domain rrtype) rawMsg = do
  yieldAnswer q rawMsg
    `mplus` yieldAuthority q rawMsg
    `mplus` yieldAdditional q rawMsg
  return ()

-- most importantly for practical purposes this is needed to extract
-- the NS AUTHORITY records which allow a delegation to take place,
-- which cannot happen just from ANSWER records.
-- so for now, let's just deal with that

-- but we also need the ADDITIONAL section in order to get glue
-- A records...
-- XXX so at this point, just implement proper shredding of the rawMsg...?

yieldAnswer :: RecursiveLookup -> DNS.DNSMessage -> Action ()
yieldAnswer q@(RecursiveLookup domain rrtype) rawMsg = do
        let correct r = DNS.rrtype r == rrtype
        let toRData = map DNS.rdata . filter correct . DNS.answer
        let syntheticResult = DNS.fromDNSMessage rawMsg toRData
        -- so we can yield the whole rrset/error now - this should not be
        -- unpacked into individual records because sometimes we should
        -- reason about the set as a whole.

        let syntheticResultSorted = fmap sort syntheticResult
        call $ Log $ "yieldAnswer: RecursiveLookup(" ++ unpack domain ++ "/" ++ show rrtype ++ ") => " ++ show syntheticResultSorted
        call $ Yield q syntheticResultSorted
        return ()

yieldAuthority :: RecursiveLookup -> DNS.DNSMessage -> Action ()
yieldAuthority q rawMsg = yieldSection DNS.authority q rawMsg

yieldAdditional :: RecursiveLookup -> DNS.DNSMessage -> Action ()
yieldAdditional q rawMsg = yieldSection DNS.additional q rawMsg

eqNameType :: DNS.ResourceRecord -> DNS.ResourceRecord -> Bool
eqNameType a b = (DNS.rrname a == DNS.rrname b) && (DNS.rrtype a == DNS.rrtype b)

ordNameType :: DNS.ResourceRecord -> DNS.ResourceRecord -> Ordering
ordNameType a b = compareOn DNS.rrname a b
               <> compareOn (DNS.typeToInt . DNS.rrtype) a b 
  where compareOn f a b = compare (f a) (f b)

yieldSection :: (DNS.DNSMessage -> [DNS.ResourceRecord]) -> RecursiveLookup -> DNS.DNSMessage -> Action ()
yieldSection section q@(RecursiveLookup domain rrtype) rawMsg = do
  assertNormalised domain
  let rrs = section rawMsg :: [DNS.ResourceRecord]

  -- rrs might be a motley pool of different name/rrtypes
  -- group these into name,rrtype equivalence classes
  let rrs' = sortBy ordNameType rrs
  let rrEquivalenceClasses = groupBy eqNameType rrs'

  call $ Log $ "yieldSection: Resource record groups are: " ++ show rrEquivalenceClasses

  (q, a) <- pick $ map (resourceRecordsToQA) rrEquivalenceClasses
  let (RecursiveLookup d1 _) = q
  assertNormalised d1
  call $ Log $ "yieldSection: Yielding " ++ show q ++ " => " ++ show a
  call $ Yield q a
  return ()
-- TODO: handle when rawMsg is an error...
-- we'd expect the data in the addition/authority sections to contain
-- eg an NS that is the "proof" of non-existence? in which case we still
-- want to capture those NS values and use them.

-- | this assumes the rrset is nonempty.
resourceRecordsToQA :: [DNS.ResourceRecord] -> (RecursiveLookup, Answer RecursiveLookup)
resourceRecordsToQA rrset = let
  canonical = head rrset
  q = RecursiveLookup (DNS.rrname canonical) (DNS.rrtype canonical)
  a = Right $ sort $ map DNS.rdata rrset
  in (q, a)


splitByZone :: DNS.Domain -> Action DNS.Domain
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

  let reassembled' = if length reassembled == 0 || (head $ reverse reassembled) /= '.'
                     then reassembled ++ "."
                     else reassembled
 
  let packed = pack reassembled'

  call $ Log $ "splitByZone: ancestor: " ++ show packed

  return packed

getNameserverForZone :: DNS.Domain -> Action DNS.Domain
getNameserverForZone zoneName = do
  call $ Log $ "getNameserverForZone: zone " ++ show zoneName
  
  resp <- call $ Launch $ RecursiveLookup zoneName DNS.NS
  call $ Log $ "getNamserverForZone: response " ++ show resp
  case resp of
    Left _ -> call End -- this path won't give us a nameserver name
    Right ns_rrset -> do 
      call $ Log $ "getNameserverForZone: zone " ++ show zoneName
                ++ " has NS RRSet " ++ show ns_rrset
  -- this might fail, rather than return a nameserver name
  -- so maybe we need to deal with that error being returned.
  -- perhaps we should actually return error here, and then
  -- turn that into more generally the resolution failing?
  -- (but not killing dugnutt...)
      (DNS.RD_NS ns_name) <- pick ns_rrset

      call $ Log $ "getNameserverForZone: zone " ++ show zoneName
                ++ " has nameserver " ++ show ns_name 
      assertNormalised ns_name
      return ns_name

-- TODO: returns (non-deterministically) the A/AAAA records for the given zone
getAddressForHost :: DNS.Domain -> Action IPv4
getAddressForHost hostName = do
  assertNormalised hostName
  call $ Log $ "getAddressForHost: looking up address for host " 
            ++ show hostName
  resp <- call $ Launch $ RecursiveLookup hostName DNS.A
  case resp of 
    Left _ -> call End
    Right a_rrset -> do
      call $ Log $ "getAddressForHost: host " ++ show hostName
                ++ " has A RRSet " ++ show a_rrset
      (DNS.RD_A a) <- pick a_rrset

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

