
-- | functions converting between RRs and textual representation
module Dugnutt.RRText where

import Network.DNS.Types (TYPE(..))

-- | like a Read instance for TYPE. This is not exhaustive.
-- This function should probably become more liberal in what
-- it accepts - for example, allowing mixed case
readTYPE :: String -> TYPE
readTYPE "A" = A
readTYPE "SOA" = SOA
readTYPE "NS" = NS
readTYPE s = error $ "readTYPE cannot convert " ++ s
