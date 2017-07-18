module Dugnutt.Domain where

import Control.Monad (when)
import Network.DNS (Domain)

import Dugnutt.Query

import Data.ByteString.Char8 (unpack)


assertNormalised :: Domain -> Action ()
assertNormalised domain = do
  when (((head . reverse . unpack) domain) /= '.') $ error "ASSERTION FAILURE in assertNormalised"
  return ()


