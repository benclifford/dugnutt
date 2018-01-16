module Main where

import Control.Monad (when, void)
import Data.ByteString.Char8 (pack)
import Data.List (isSuffixOf)
import Data.Monoid (mempty)
import qualified Options.Applicative as OA

import Dugnutt (initq)

import Dugnutt.RecursiveLookup (RecursiveLookup (..))

import Dugnutt.RRText

import Network.DNS (Domain, TYPE (..))

main :: IO ()
main = do

  os <- OA.execParser opts
  cliProgress os "dugnutt is copyright 2017-2018 Ben Clifford"
  let query = RecursiveLookup (_domain os) (_type os)
  answers <- initq (_verbose os) query

  void $ mapM printLn answers

  cliProgress os "finished"

cliProgress :: DugnuttCLIOpts -> String -> IO ()
cliProgress os msg = do
  when (_verbose os) $ putStrLn $ "dugnutt cli: " ++ msg

stringToDotNormalisedDomain :: String -> Domain
stringToDotNormalisedDomain s =
  if "." `isSuffixOf` s
    then pack s
    else pack (s ++ ".")


printLn :: Show v => v -> IO ()
printLn = putStrLn . show


opts :: OA.ParserInfo DugnuttCLIOpts
opts = OA.info optParser mempty

data DugnuttCLIOpts = DugnuttCLIOpts {
    _domain :: Domain
  , _type :: TYPE
  , _verbose :: Bool
  }

optParser :: OA.Parser DugnuttCLIOpts
optParser =  DugnuttCLIOpts <$> domainOpt <*> typeOpt <*> verboseOpt

domainOpt :: OA.Parser Domain
domainOpt = stringToDotNormalisedDomain <$> OA.strArgument (OA.metavar "DOMAIN")

typeOpt :: OA.Parser TYPE
typeOpt = readTYPE <$> OA.strArgument (OA.metavar "TYPE")

verboseOpt :: OA.Parser Bool
verboseOpt = OA.switch (OA.long "verbose")

