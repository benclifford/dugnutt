{-# Language TypeFamilies #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

{-# Options_GHC -fwarn-incomplete-patterns #-}

-- | This module contains the language for writing Dugnutt queries.


module Dugnutt.Query where

import Dugnutt.FFree
import Dugnutt.Loggable

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.List (sortBy, groupBy, partition)
import Data.Typeable (Typeable, cast)
import Data.Void

class (Show q, Show (Answer q),
       Typeable q, Typeable (Answer q),
       Eq q, Eq (Answer q)
      )
  => Query q where

  -- | Queries of type q will return answers of type (Answer q)
  type Answer q

  -- | How to launch a query
  launch :: q -> Action ()

type Action = FFree DugnuttCommand

-- | The language of Dugnutt
data DugnuttCommand v where

  -- |Log a message
  Log :: String -> DugnuttCommand ()

  -- |perform some IO action
  LiftIO :: IO v -> DugnuttCommand v

  -- | Yield an answer to a specified query, which does
  --   not have to be the query that is currently being
  --   executed (if there is such).
  --   QUESTION/DISCUSSION: Yield could 'return' Void and
  --   so indicate in the type system that it stops the
  --   flow of control rather than continuing - so that
  --   multiple yields have to happen over mpluses rather
  --   than the current serialisation, which makes me
  --   feel a bit uncomfortable non-det-wise.
  Yield :: Query q => q -> Answer q -> DugnuttCommand Void

  -- | Launch a query. Similar discussion about Void return
  --   type as with Yield.
  Launch :: Query q => q -> DugnuttCommand (Answer q)

  Fork :: DugnuttCommand Bool
  End :: DugnuttCommand t -- ends the current thread without a result

-- only freers which implement some monad plus effect have
-- this, so specialise this to our particular freer instance
instance Alternative Action where
  empty = call End
  a <|> b = do
    choice <- call Fork
    if choice then a else b

instance MonadPlus Action

-- only freers which sit on top of IO have this, so specialise
-- it to our instance
instance MonadIO Action where
  liftIO act = call (LiftIO act)

-- An action to call a Dugnutt command
call :: DugnuttCommand v -> Action v
call cmd = Impure cmd pure


-- interprets an Action in the form of something to be dealt
-- with by a lower level processor which will get a value to stop
-- at (on the Right), or somehow a set of new tasks that we need
-- to deal with (which might be launches or might be new results)
-- Initially, new results (from Yield) are what I need to implement.

runAction :: (Loggable m, MonadIO m) => Action v -> m [Next]
runAction (Pure _) = return [] -- discard the result. and there are
                               -- no more actions to perform.

runAction (Impure (Log msg) k) = do
  logMsg $ "; " ++ msg
  runAction $ k ()

runAction (Impure (LiftIO a) k) = do
  v <- liftIO a
  runAction $ k v

runAction (Impure (Yield q a) _k) = do
  traceInterpreter "Yield: running rest of program ..."
  return [(Y q a)]

runAction (Impure (Launch q) k) = do
  traceInterpreter $ "Launch: queuing query: " ++ show q
  -- we should register the query to be launched
  -- but also register the continuation callback
  -- appropriately: both to catch future yields
  -- and to deal with already yielded values.
  -- nexts' <- runAction $ k ()
  return [(L q), (C q (\v -> ((k v) >> call End)))]

runAction (Impure (Fork) k) = do
  traceInterpreter "Forking: False-side"
  nextsFalse <- runAction (k False)
  traceInterpreter "Forking: True-side"
  nextsTrue <- runAction (k True)
  return $ nextsFalse ++ nextsTrue

runAction (Impure End _k) = do
  traceInterpreter "End: aborting this thread and ignoring continuation"
  return []

-- | drive something (an Action?) until there are no
--   nexts left to do.
drive :: (Loggable m, MonadIO m, Query q) => [DBEntry] -> q -> m [DBEntry]
drive db query = do
  traceInterpreter $ "drive: Driving " ++ show query
  driveIter [L query] db


driveIter :: (Loggable m, MonadIO m) => [Next] -> [DBEntry] -> m [DBEntry]
driveIter todo@[] db = do
  printStats todo db
  traceInterpreter $ "driveIter: STEP: all done with database size " ++ (show . length) db
  printStats' todo db
  return db

driveIter todo@((L query):ns) db = do
  printStats todo db
  traceInterpreter $ "driveIter: STEP: Request to launch a query: " ++ show query
  if not (isLaunched db query)
    then do
      traceInterpreter "driveIter: Query has not been previously launched. Launching now."
      ns' <- runAction (launch query)
      traceInterpreter $ "driveIter: Action produced "
              ++ (show . length) ns'
              ++ " additional nexts."
      let db' = shrinkWholeDB $ updateDBWithLaunch db query
      driveIter (ns' ++ ns) db'
    else do
      traceInterpreter "driveIter: Query has previously launched. Not re-launching."
      driveIter ns db
  -- the ordering of this concatenation is going to
  -- have some influence on whether we behave vaguely
  -- breadth first or depth first.

-- TODO: only yield if this value isn't already in the
-- database. if it is in the database, this means that
-- all necessary callbacks should have already been called,
-- either by the previous Yield, or by the 'C' callback.
driveIter todo@((Y query answer):ns) db = do
  printStats todo db
  traceInterpreter "driveIter: STEP: yield"

  if not (isAlreadyKnown db query answer)
    then do 
      traceInterpreter "driveIter: new answer:"
      traceInterpreter $ (show query) ++ " => " ++ (show answer)

      let cbs = findCallbacks db query

      traceInterpreter $ "driveIter: yield: there are " ++ (show . length) cbs
              ++ " callbacks for this query."  

      nexts' <- mapM (\k -> runAction (k answer)) cbs

      let nexts = concat nexts'

      let db' = shrinkWholeDB $ updateDBWithYield db query answer
      let ns' = nexts ++ ns
      driveIter ns' db'
    else do
      traceInterpreter $ "driveIter: answer already known: "
        ++ (show query) ++ " => " ++ (show answer)
      driveIter ns db
     

-- | C will:
--     i) call the specified callback with any relevant answers
--        straight away
--    ii) store the callback for later use when new relevant
--        answers are Yielded.
driveIter todo@((C q k):ns) db = do
  printStats todo db
  traceInterpreter "driveIter: STEP: callback"
  traceInterpreter $ "driveIter: adding callback for " ++ (show q)

  let anss = findAnswers db q
  traceInterpreter $ "driveIter: callback: there are " ++ (show . length) anss
          ++ " existing answers for this callback"

  nexts' <- mapM (runAction . k) anss

  let nexts = concat nexts'

  let db' = shrinkWholeDB $ updateDBWithCallback db q k
  let ns' = nexts ++ ns
  driveIter ns' db'


data Next where
  -- | yield a result for a query
  Y :: Query q => q -> Answer q -> Next

  -- | launch a query
  L :: Query q => q -> Next

  -- | callback for query answers
  C :: Query q => q -> (Answer q -> Action Void) -> Next

-- | This is not parameterised by the type of query,
--   so that a collecton of DBEntries can represent
--   different types of queries.
--   An entry will contain both the answers for this
--   query already encountered, callbacks to be
--   run for new answers, and a flag whether this
--   query has been launched or not. (it might not be
--   because we might have got an answer through some
--   other process)
data DBEntry where
  DBEntry :: Query q => q -> [Answer q] -> [Answer q -> Action Void] -> Bool -> DBEntry

-- | Projects out the query from a database entry.
--   Note that, depending on the return type, this
--   might return Nothing - if the return type is
--   not compatible with the type stored in the
--   DBEntry
--   TODO: can this be implemented as a regular record
--   syntax accessor, or does the polymorphism break it?
getQuery :: Query q => DBEntry -> Maybe q
getQuery (DBEntry q' _as _cs _l) = cast q'


updateDBWithYield :: Query q => [DBEntry] -> q -> Answer q -> [DBEntry]
updateDBWithYield db query answer = 
  updateDB db query [answer] [] False

updateDBWithCallback :: Query q => [DBEntry] -> q -> (Answer q -> Action Void) -> [DBEntry]
updateDBWithCallback db query k = 
  updateDB db query [] [k] False

updateDBWithLaunch :: Query q => [DBEntry] -> q -> [DBEntry]
updateDBWithLaunch db query = 
  updateDB db query [] [] True

updateDB :: Query q => [DBEntry] -> q -> [Answer q] -> [Answer q -> Action Void] -> Bool -> [DBEntry]
updateDB db query a k l = 
  let (these, others) = partition p db
      p dbe = getQuery dbe == Just query
      these' = case these of
        [] -> [DBEntry query a k l]
        ((DBEntry _ a' k' l') : xs) -> let
               k'' = fromCastedJust $ cast k'
               a'' = fromCastedJust $ cast a'
           in (DBEntry query (a ++ a'') (k ++ k'') (l || l')) : xs
    in these' ++ others


instance Show DBEntry where
  show (DBEntry query anss ks launched) =
       "DBEntry { q = "
    ++ show query
    ++ ", n_ans = " ++ (show . length) anss
    ++ ", ans = " ++ show anss
    ++ ", n_callback = " ++ (show . length) ks
    ++ ", " ++ if launched then "launched" else "not launched"
    ++ "}"

findAnswers :: Query q => [DBEntry] -> q -> [Answer q]
findAnswers db q = let
     getAnswers (DBEntry _q' as _cs _l) = fromCastedJust $ cast as
     p dbe = getQuery dbe == Just q
  in concat $ map getAnswers $ filter p db 

findCallbacks :: Query q => [DBEntry] -> q -> [Answer q -> Action Void]
findCallbacks db q = let
     getCallbacks (DBEntry _q' _as cs _l) = fromCastedJust $ cast cs
     p dbe = getQuery dbe == Just q
  in concat $ map getCallbacks $ filter p db

isLaunched :: Query q => [DBEntry] -> q -> Bool
isLaunched db q = let
     getLaunched (DBEntry _q' _as _cs l) = l
     p dbe = getQuery dbe == Just q
  in or $ map getLaunched $ filter p db

isAlreadyKnown :: Query q => [DBEntry] -> q -> Answer q -> Bool
isAlreadyKnown db q a = let
  answers = findAnswers db q
  in a `elem` answers

countLaunched :: [DBEntry] -> Int
countLaunched db = let
      getLaunched (DBEntry _ _ _ l) = l
      p dbe = getLaunched dbe
  in length $ filter p db

countCallbacks :: [DBEntry] -> Int
countCallbacks db = let
      getEntryCallbackLength (DBEntry _ _ cs _) = length cs
  in sum (map getEntryCallbackLength db)

countAnswers :: [DBEntry] -> Int
countAnswers db = let
      getEntryAnswerLength (DBEntry _ as _ _) = length as
  in sum (map getEntryAnswerLength db)

-- TODO: countQueries would be nice too...
-- might need some casting fun.
-- this would be in the same 'scale' as the
-- number of launches that we've made, but would differ
-- because would be queries we know facts for (so would
-- be >=?)

-- | optionally print stats at some points, although current
--   implementation does nothing - this is a programmer time
--   verbosity choice. TODO: make this runtime choosable
printStats :: (MonadIO m) => [Next] -> [DBEntry] -> m ()
printStats _nexts _db = return ()

printStats' :: (Loggable m) => [Next] -> [DBEntry] -> m ()
printStats' nexts db = logMsg $ 
     "; *** "
  ++ (show . length) nexts
  ++ " steps to do. "
  ++ (show . length) db
  ++ " entries in database: "
  ++ (show . countLaunched) db
  ++ " queries launched, "
  ++ (show . countCallbacks) db
  ++ " callbacks registered, "
  ++ (show . countAnswers) db
  ++ " answers known. ***"
 
traceInterpreter :: Loggable m => String -> m ()
traceInterpreter s = logMsg s

-- | given a database, return a new database that
--   contains the same data, but hopefully better
--   laid out - with more shared DBEntries.
--   or perhaps it makes no change: it is a
--   (programmer-time) optimisation choice
--   about whether to perform the shrink or not.
--   TODO: make this optimisation choices runtime
--   choosable
shrinkWholeDB :: [DBEntry] -> [DBEntry]
shrinkWholeDB = id

-- | given a database, return a new database that contains
--   the same data, but maybe better laid out. see
--   'shrinkWholeDB' which is called at a different point but
--   is in the same spririt.
shrinkDB' :: [DBEntry] -> [DBEntry]
shrinkDB' db = map mergeEntries $ groupBy eqOnQuery $ sortBy ordOnQuery db
  where

    -- This is icky, but basically uses show to convert arbitrary query
    -- types all into one single type that has Ord. It doesn't matter
    -- what the ordering is, as long as it puts the same queries next to
    -- each other. The show based implementation relies on different
    -- queries serialising to different strings.
    ordOnQuery a b = (sq a) `compare` (sq b)

    -- This uses show based ordering for consistency with ordOnQuery,
    -- but eqOnQuery is probably easy to implement using cast (at least
    -- compared to doing so for ordOnQuery, where an ordering on types
    -- is needed)
    eqOnQuery a b = (sq a) == (sq b)

    sq (DBEntry q _ _ _) = show q

    -- Assuming that all the given entries are for the same query
    -- (which is not checked), create a single DB entry.
    mergeEntries :: [DBEntry] -> DBEntry
    mergeEntries entries = foldr1 catEntries entries

    catEntries :: DBEntry -> DBEntry -> DBEntry
    catEntries (DBEntry q1 a1 b1 l1) (DBEntry _q2 a2 b2 l2) = let
      a' = a1 ++ (fromCastedJust $ cast a2)
      b' = b1 ++ (fromCastedJust $ cast b2)
      l' = (l1 || l2)
      in DBEntry q1 a' b' l'

-- | this is fromJust, but with a different error message
--   that reflects the belief that it should never hit the
--   Nothing case.
fromCastedJust :: Maybe v -> v
fromCastedJust (Just x) = x
fromCastedJust Nothing = error "impossible: fromJust called on Nothing"

