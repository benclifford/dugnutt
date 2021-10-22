module Main where

import Control.Monad (when, void)
import Data.ByteString.Char8 (pack)
import Data.List (isSuffixOf)
import qualified Options.Applicative as OA

import Dugnutt (initq)

import Dugnutt.RecursiveLookup (RecursiveLookup (..))

import Dugnutt.RRText

import Network.DNS (Domain, TYPE (..), DNSError, RData)

main :: IO ()
main = do

  os <- OA.execParser opts
  cliProgress os "dugnutt is copyright 2017-2018 Ben Clifford"
  let query = RecursiveLookup (_domain os) (_type os)
  answers <- initq (_verbose os) query

  putStrLn $ "There are " ++ (show . length) answers ++ " results:"
  void $ mapM printAnswer (answers `zip` [1..])

  when (length answers /= 1 && _checkUnique os) $
    error "Results are not unique"

  cliProgress os "finished"


printAnswer :: (Either DNSError [RData], Int) -> IO ()
printAnswer (answer, n) = do
  putStrLn $ "Result " ++ (show n) ++ ": "
  case answer of
    Right good -> do putStrLn "Good answer:"
                     print good
    Left bad -> do putStrLn "Error:"
                   print bad
  putStrLn "---"

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
  , _checkUnique :: Bool
  }

optParser :: OA.Parser DugnuttCLIOpts
optParser =  DugnuttCLIOpts <$> domainOpt <*> typeOpt <*> verboseOpt <*> checkUniqueOpt

domainOpt :: OA.Parser Domain
domainOpt = stringToDotNormalisedDomain <$> OA.strArgument (OA.metavar "DOMAIN")

typeOpt :: OA.Parser TYPE
typeOpt = readTYPE <$> OA.strArgument (OA.metavar "TYPE")

verboseOpt :: OA.Parser Bool
verboseOpt = OA.switch (OA.long "verbose")

checkUniqueOpt :: OA.Parser Bool
checkUniqueOpt = OA.switch (OA.long "check-unique")
