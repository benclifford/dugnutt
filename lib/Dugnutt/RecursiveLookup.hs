{-# Language TypeFamilies #-}
{-# OPTIONS_GHC -Werror #-}
module Dugnutt.RecursiveLookup where

import Control.Monad (mplus, msum)
import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)
import Data.Function (on)
import Data.Monoid ( (<>) )
import Data.IP
import Data.List (groupBy, sort, sortBy, tails, intersperse)
import Data.Void (vacuous, Void)

import qualified Network.DNS as DNS

import Dugnutt.Actions (pick)
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

    resolveFromRoot q `mplus` speculativeResolve q

-- For now, resolveFromRoot and speculativeResolve are basically
-- the same, but possibly later, stricter error handling will happen
-- in resolveFromRoot.

resolveFromRoot :: RecursiveLookup -> Action ()
resolveFromRoot q = do
  -- To begin with, caches aside, all we know is that
  -- the root is an ancestor (because it is an ancestor of all
  -- names).
  let ancestorZone = pack "."
  assertNormalised ancestorZone

  nameserverHostname <- getNameserverForZone ancestorZone
  assertNormalised nameserverHostname

  call $ Launch (RecursiveLookupFromNameserver q nameserverHostname)
  return ()

speculativeResolve :: RecursiveLookup -> Action ()
speculativeResolve q = do
  possibleAncestorZone <- splitByZone (domain q)
  assertNormalised possibleAncestorZone

  nameserverHostname <- getNameserverForZone possibleAncestorZone
  assertNormalised nameserverHostname

  call $ Launch (RecursiveLookupFromNameserver q nameserverHostname)

  return ()

-- | note that this never "returns" any answers (that is, yields
--   for a RecursiveLookupFromNameserver query). That is possibly bad
--   if we want to do anything using function syntax for such a call,
--   or reason functionally.
data RecursiveLookupFromNameserver = RecursiveLookupFromNameserver {
    query :: RecursiveLookup,
    nameserver :: DNS.Domain
  } deriving (Eq, Show)

instance Query RecursiveLookupFromNameserver where
  type Answer RecursiveLookupFromNameserver = Either DNS.DNSError [DNS.RData]

  launch q' = do
    let q = query q'
    let nameserverHostname = nameserver q'
    nameserverAddress <- getAddressForHost nameserverHostname
    call $ Log $ "recursiveLookupFromNameserver(" ++ unpack (domain q) ++ "/" ++ show (rrtype q) ++ ") nameserver: " ++ show nameserverHostname ++ "  nameserverAddress " ++ show nameserverAddress

    res <- call $ Launch (QSA.QuerySpecificAddress (show nameserverAddress) (domain q) (rrtype q))

    -- There are various results we can get here, and which I need to
    -- decide how to represent in the internal structures:
    --  1. a server error (res = Left err)
    --  2. a final successful result without CNAME
    --  3. a delegation
    --  4. NXDOMAIN: status: NXDOMAIN, authority section contains SOA of containing zone. see rfc1034/s4.3.4 - the SOA is optional but allows caching. so status = NXDOMAIN is how we know this case 4 is matched, not the SOA.
    --     TODO: Note that this asserts that the domain does not exist, for *all* RRtypes - not just the requested type. So for purposes of exploring all routes, we should cache/discover this fact whenever we ask for a different rrtype on the same domain, in addition to the present handling. (use case: buggy server asserts a name does not exist for type X, then we request type Y and it returns a result: depending on cache ordering, we either know the result for Y, or we know that Y does not exist).
    --  5. a CNAME: status NOERROR. ANSWER contains 'domain CNAME s2'. 
    --   5a. possibly contains 'd2 ...' RRs for the original type (eg if the zone contains it).   
    --   and there is an AUTHORITY of NSes and ADDITIONAL data for NS As. (www.hawaga.org.uk for example); 
    --   5b. sometimes neither (www.1stmerrow.org.uk for example) - I'm not sure why?
    --   5c. With 'merrowscouts.hawaga.org.uk', a delegation into another zone but that is served from the same server, the onwards biscay.cqx.ltd.uk record is added to the ANSWER section, and the AUTHORITY section contains NS records for cqx.ltd.uk and the ADDITIONAL section contains A records for those NSes.
    --   6. an indication that a label exists but has no RRs of the specified type. eg paella.hawaga.org.uk/RP: status NOERROR, nothing in ANSWER section, authority section contains SOA for the containing zone. This latter clause is how to tell it apart from a delegation, I think, and we should check that the authority contains no NS RRs to extra clear about separating cases 3 and 6.
    -- There are two different ways that this could be handled at a higher level: one that the RR set is empty (which means in places where I care, for the purposes of non-determinism, about checking whether an rrset is empty or not so as to treat it like a NameError, those checks need to be explicit). The second is to treat the error like a NameError (or something similar). 
    --   7. when we requested an ANY, what comes back in the 2. case is a bunch of resource records of assorted types, rather than an "ANY" type. A simple list of [RData] is insufficient for representing this. Maybe it shouldn't be allowed at this level? (although artificially generated to get more results)

    case res of

      -- case 2
      Right rawMsg
        | (DNS.rcode . DNS.flags . DNS.header) rawMsg == DNS.NoErr
          && rawMessageContainsAnswers q rawMsg
        -> vacuous $ yieldRawMessage rawMsg

      -- case 3: delegation
      -- Note that this does not check if the referral is a downward
      -- delegation (rather than some kind of upwards, towards the root
      -- referral, which are rare in practice). For that, we'd also
      -- need to pay attention to 'possibleAncestorZone'.
      -- TODO: also check that there are no SOAs in the authority section
      -- to be clear that it is not a denial of existence (see clause 6)
      -- TODO: if delegated NSes are children of the zone, validate the
      -- glue - that is, check that if there are in-zone NSes, that glue
      -- is specified (and works - whatever that means) - so that we can
      -- detect missing glue even if we've got cached records somehow.
      -- This todo is probably pretty lowpri?
      Right rawMsg
        | (null . DNS.answer) rawMsg
          && hasReferralNS q rawMsg
          && (DNS.rcode . DNS.flags . DNS.header) rawMsg == DNS.NoErr
        -> (vacuous $ yieldRawMessage rawMsg)
           `mplus` (vacuous $ recurseOnNameservers q rawMsg)

      -- case 1
      Left err -> vacuous (call $ Yield q (Left err))

      -- case 4
      Right rawMsg | (DNS.rcode . DNS.flags . DNS.header) rawMsg == DNS.NameErr -> vacuous (call $ Yield q (Left DNS.NameError))
        `mplus` (vacuous $ yieldRawMessage rawMsg)

      -- case 6 - a label exists, but there are no RRs of the specified type.
      Right rawMsg
        | (DNS.rcode . DNS.flags . DNS.header) rawMsg == DNS.NoErr 
          && (null . DNS.answer) rawMsg
          && hasAuthoritySOA rawMsg
        -> vacuous  (call $ Yield q (Right []))
           `mplus` (vacuous $ yieldRawMessage rawMsg)

      -- case 5 - CNAME: what comes back has the right rrname, but an
      --          rrtype of CNAME. The top level resolution algorithm
      --          should restart, looking up the new name and the
      --          original rrtype.
      --          For provenance (and other?) purposes, I'd like to
      --          be able to observe this CNAME indirection later,
      --          rather than it being invisible - for example, to
      --          check that NS records are not pointing at CNAMEs.
      --          This might necessitate another layer of query
      --          processing functions.

      Right rawMsg
        | (DNS.rcode . DNS.flags . DNS.header) rawMsg == DNS.NoErr
          && hasRelevantCNAME q rawMsg
          && rrtype q /= DNS.CNAME
        -> vacuous (yieldRawMessage rawMsg)
           `mplus` recurseOnCNAME q rawMsg

      -- fail on all other situations, rather than guessing what is
      -- going on.
      Right other -> error $ "RecursiveLookup: cannot handle this response: " ++ show other


-- | Gets all the NSes from rawMessage, and recursively calls
--   recursiveLookupFromNameserver
recurseOnNameservers :: RecursiveLookup -> DNS.DNSMessage -> Action Void
recurseOnNameservers q rawMsg = msum $
           map
             (\nsName -> (call $ Launch (RecursiveLookupFromNameserver q nsName)) >> terminate)
             nsNames
  where nsNames :: [DNS.Domain]
        nsNames = (getNSes . filter correctRR . DNS.authority) rawMsg

        correctRR :: DNS.ResourceRecord -> Bool
        correctRR rr = DNS.rrtype rr == DNS.NS

        getNSes :: [DNS.ResourceRecord] -> [DNS.Domain]
        getNSes = map (getNS . DNS.rdata)

        getNS :: DNS.RData -> DNS.Domain
        getNS (DNS.RD_NS ns_name) = ns_name

        terminate :: Action Void
        terminate = call End

-- | Recursively calls RecursiveLookup on a CNAME that has been
--   supplied as an answer.
recurseOnCNAME q rawMsg = do

  let rcnames = relevantCNAMEs q rawMsg
  -- TODO: warn if not exactly one CNAME
 
  let rcname_rdatas = DNS.rdata <$> rcnames

  -- This should basically always be exactly one
  -- choice, but if not, it'll do them all.
  rcname_rdata <- pick rcname_rdatas

  let (DNS.RD_CNAME newDomain) = rcname_rdata

  let newQuery = RecursiveLookup {
                   domain = newDomain
                 , rrtype = rrtype q
                 }
  res <- call $ Launch newQuery

  -- transcribe result from the recursed query into results for
  -- the original query
  vacuous (call $ Yield q res)

-- | Does the raw message contain a CNAME answer to the supplied
--   query?
hasRelevantCNAME q rawMsg = let
  as' = relevantCNAMEs q rawMsg
  in (not . null) as'

relevantCNAMEs q rawMsg = let
    correctRR rr = DNS.rrname rr `eqDNSNormalised` (domain q)
                && DNS.rrtype rr == DNS.CNAME 
    as = DNS.answer rawMsg
    in filter correctRR as

-- | Does the raw message contain resource record answers that
--   match the given query?
rawMessageContainsAnswers q rawMsg = let

  correctName rr = DNS.rrname rr `eqDNSNormalised` domain q
  correctRR rr = DNS.rrtype rr == rrtype q

  -- as will contain the answer resource records which may
  -- be for different names and RRtypes, not necessarily the
  -- given query: for example CNAME, or RRSIG.
  as = DNS.answer rawMsg
  as' = filter correctName as
  as'' = filter correctRR as'
  in (not . null) as''

hasReferralNS q rawMsg = let

  correctRR rr = DNS.rrtype rr == DNS.NS

  as = DNS.authority rawMsg

  -- TODO: should check there that the NS records are for an
  -- appropriate authority - something relevent to the referral
  -- query.
  as' = filter correctRR as
  in (not . null) as'

-- | Checks that the authority field contains an SOA.
-- TODO: also check that the SOA records are for an appropriate
-- authority - something relevant to the query
hasAuthoritySOA rawMsg = let
  correctRR rr = DNS.rrtype rr == DNS.SOA
  as = DNS.authority rawMsg
  as' = filter correctRR as
  in (not . null) as'

-- | This will yield all RRsets found in answer, authority and
--   additional sections, regardless of which query was asked.
yieldRawMessage :: DNS.DNSMessage -> Action Void
yieldRawMessage rawMsg = do
  yieldAnswer rawMsg
    `mplus` yieldAuthority rawMsg
    `mplus` yieldAdditional rawMsg

yieldAnswer :: DNS.DNSMessage -> Action Void
yieldAnswer rawMsg = yieldSection DNS.answer rawMsg

yieldAuthority :: DNS.DNSMessage -> Action Void
yieldAuthority rawMsg = yieldSection DNS.authority rawMsg

yieldAdditional :: DNS.DNSMessage -> Action Void
yieldAdditional rawMsg = yieldSection DNS.additional rawMsg

eqNameType :: DNS.ResourceRecord -> DNS.ResourceRecord -> Bool
eqNameType a b = (DNS.rrname a `eqDNSNormalised` DNS.rrname b) && (DNS.rrtype a == DNS.rrtype b)

ordNameType :: DNS.ResourceRecord -> DNS.ResourceRecord -> Ordering
ordNameType a b = (compare `on` (dnsNormalise . DNS.rrname)) a b
               <> (compare `on` (DNS.typeToInt . DNS.rrtype)) a b 

eqDNSNormalised :: DNS.Domain -> DNS.Domain -> Bool
eqDNSNormalised = (==) `on` dnsNormalise

-- | Normalises the case of domain to upper case. This relies on
--   Data.Char.toUpper performing this in the way required by
--   DNS standards.
dnsNormalise :: DNS.Domain -> DNS.Domain
dnsNormalise = BS8.map toUpper

yieldSection :: (DNS.DNSMessage -> [DNS.ResourceRecord]) -> DNS.DNSMessage -> Action Void
yieldSection section rawMsg = do
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

getNameserverForZone :: DNS.Domain -> Action DNS.Domain
getNameserverForZone zoneName = do
  call $ Log $ "getNameserverForZone: zone " ++ show zoneName
  
  resp <- call $ Launch $ RecursiveLookup zoneName DNS.NS
  call $ Log $ "getNameserverForZone: response " ++ show resp
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

-- | Returns (non-deterministically) the A records for the given zone
-- and TODO: should do AAAA records
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

