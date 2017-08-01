{-# Language OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (pack)
import Data.List (isSuffixOf)
import System.Environment (getArgs)

import Dugnutt (initq)

import Dugnutt.RecursiveLookup (RecursiveLookup (..))

import Network.DNS (Domain, TYPE (..))

main :: IO ()
main = do
  cliProgress "main start"

  args@[domainS, typeS] <- getArgs

  let domain = readDomain domainS
  let ty = read typeS

  let query = RecursiveLookup domain ty
  initq query
  cliProgress "main end"

cliProgress :: String -> IO ()
cliProgress msg = do
  putStr $ "dugnutt cli: " ++ msg

readDomain :: String -> Domain
readDomain s =
  if "." `isSuffixOf` s
    then pack s
    else pack (s ++ ".")
