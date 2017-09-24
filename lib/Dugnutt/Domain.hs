{-# OPTIONS_GHC -Werror -Wall #-}

module Dugnutt.Domain where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)
import Data.Function (on)
import Data.Monoid ( (<>) )
import Network.DNS (Domain)
import qualified Network.DNS as DNS

import Dugnutt.Actions
import Dugnutt.Query

import Data.ByteString.Char8 (pack, unpack)
import Data.List (tails, intersperse)
import Data.List.Split (splitOn)

assertDotNormalised :: Domain -> Action ()
assertDotNormalised domain = do
  when (((head . reverse . unpack) domain) /= '.') $ error "ASSERTION FAILURE in assertNormalised"
  return ()

eqNameType :: DNS.ResourceRecord -> DNS.ResourceRecord -> Bool
eqNameType a b = (DNS.rrname a `eqDNSCaseNormalised` DNS.rrname b) && (DNS.rrtype a == DNS.rrtype b)

ordNameType :: DNS.ResourceRecord -> DNS.ResourceRecord -> Ordering
ordNameType a b = (compare `on` (dnsCaseNormalise . DNS.rrname)) a b
               <> (compare `on` (DNS.typeToInt . DNS.rrtype)) a b 

-- | Compares assuming domain name is case-normalised,
--   but not dot-normalised as used in assertNormalised.
--   TODO put 'case' or 'dot' prefix on every use of normalise(d)
eqDNSCaseNormalised :: DNS.Domain -> DNS.Domain -> Bool
eqDNSCaseNormalised = (==) `on` dnsCaseNormalise

-- | Normalises the case of domain to upper case. This relies on
--   Data.Char.toUpper performing this in the way required by
--   DNS standards.
dnsCaseNormalise :: DNS.Domain -> DNS.Domain
dnsCaseNormalise = BS8.map toUpper


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

  let reassembled' = if length reassembled == 0 || (head $ reverse reassembled) /= '.'
                     then reassembled ++ "."
                     else reassembled

  let packed = pack reassembled'

  call $ Log $ "splitByZone: ancestor: " ++ show packed

  return packed

