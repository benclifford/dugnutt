{-# OPTIONS_GHC -Werror -Wall #-}

module Dugnutt.Domain where

import Control.Monad (when)
import Network.DNS (Domain)

import Dugnutt.Actions
import Dugnutt.Query

import Data.ByteString.Char8 (pack, unpack)
import Data.List (tails, intersperse)
import Data.List.Split (splitOn)

assertNormalised :: Domain -> Action ()
assertNormalised domain = do
  when (((head . reverse . unpack) domain) /= '.') $ error "ASSERTION FAILURE in assertNormalised"
  return ()


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

