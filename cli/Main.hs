{-# Language OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import Dugnutt (initq)

import Dugnutt.RecursiveLookup (RecursiveLookup (..))

import Network.DNS (TYPE (..))

main :: IO ()
main = do
  cliProgress "main start"

  args@[domainS, typeS] <- getArgs

  let domain = read domainS
  let ty = read typeS

  let query = RecursiveLookup domain ty
  initq query
  cliProgress "main end"

cliProgress :: String -> IO ()
cliProgress msg = do
  putStr $ "dugnutt cli: " ++ msg
