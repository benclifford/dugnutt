module Main where

import Data.ByteString.Char8 (pack)
import Data.List (isSuffixOf)
import Data.Monoid (mempty)
import qualified Options.Applicative as OA
import System.Environment (getArgs)

import Dugnutt (initq)

import Dugnutt.RecursiveLookup (RecursiveLookup (..))

import Dugnutt.RRText

import Network.DNS (Domain, TYPE (..))

main :: IO ()
main = do
  cliProgress "dugnutt is copyright 2017-2018 Ben Clifford"

  os <- OA.execParser opts

  let query = RecursiveLookup (_domain os) (_type os)
  initq query
  cliProgress "finished"

cliProgress :: String -> IO ()
cliProgress msg = do
  putStrLn $ "dugnutt cli: " ++ msg

stringToDotNormalisedDomain :: String -> Domain
stringToDotNormalisedDomain s =
  if "." `isSuffixOf` s
    then pack s
    else pack (s ++ ".")

opts :: OA.ParserInfo DugnuttCLIOpts
opts = OA.info optParser mempty

data DugnuttCLIOpts = DugnuttCLIOpts {
    _domain :: Domain
  , _type :: TYPE
  }

optParser :: OA.Parser DugnuttCLIOpts
optParser =  DugnuttCLIOpts <$> domainOpt <*> typeOpt

domainOpt :: OA.Parser Domain
domainOpt = stringToDotNormalisedDomain <$> OA.strArgument (OA.metavar "DOMAIN")

typeOpt :: OA.Parser TYPE
typeOpt = readTYPE <$> OA.strArgument (OA.metavar "TYPE")

