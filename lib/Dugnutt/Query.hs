{-# Language TypeFamilies #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

{-# Options_GHC -fwarn-incomplete-patterns #-}

-- | This module contains the language for writing Dugnutt queries.


module Dugnutt.Query where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Void

class (Show q, Show (Answer q)) => Query q where

  -- | Queries of type q will return answers of type (Answer q)
  type Answer q

  -- | How to launch a query
  launch :: q -> Action ()

type Action = FFree DugnuttCommand

-- | The language of Dugnutt
data DugnuttCommand v where

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
  Launch :: Query q => q -> DugnuttCommand ()

  Fork :: DugnuttCommand Bool
  End :: DugnuttCommand Void -- ends the current thread without a result

-- | from okmij "Freer monads, More Extensible Effects",
--   I've taken the freer monad stuff, but not the extensible
--   effects stuff (mostly because I didn't want to spend
--   time figuring out if I could get the Query/Answer
--   behaviour to work as an extensible effect).

data FFree f a where
  Pure :: a -> FFree f a
  Impure :: f x -> (x -> FFree f a) -> FFree f a

-- all freers have these:
instance Functor (FFree f)

instance Applicative (FFree f) where
  pure v = Pure v

instance Monad (FFree f) where
  Pure v >>= rest = rest v
  (Impure cmd k') >>= k = Impure cmd (\v -> ((k' v) >>= k))

-- only freers which implement some monad plus effect have
-- this, so specialise this to our particular freer instance
instance Alternative Action
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

runAction :: Action v -> IO [Next]
runAction (Pure v) = return [] -- discard the result. and there are
                               -- no more actions to perform.
runAction (Impure (LiftIO a) k) = do
  v <- a
  runAction $ k v
runAction (Impure (Yield q a) k) = do
  putStrLn "Yield: running rest of program ..."
  {- 
  nexts' <- runAction $ k ()
  putStrLn $ "Yield: rest of program gave "
          ++ (show . length) nexts'
          ++ " nexts."
  -}
  
  return [(Y q a)]

runAction (Impure (Launch q) k) = do
  putStrLn $ "Launch: query: " ++ show q
  nexts' <- runAction $ k ()
  return $ (L q) : nexts'

runAction (Impure (Fork) k) = do
  putStrLn "Forking: False-side"
  nextsFalse <- runAction (k False)
  putStrLn "Forking: True-side"
  nextsTrue <- runAction (k True)
  return $ nextsFalse ++ nextsTrue

runAction (Impure End k) = do
  putStrLn "End: aborting this thread and ignoring continuation"
  return []

-- | drive something (an Action?) until there are no
--   nexts left to do.
drive :: Query q => q -> IO [DBEntry]
drive query = do
  putStrLn $ "drive: Driving " ++ show query
  driveIter [L query] []


driveIter :: [Next] -> [DBEntry] -> IO [DBEntry]
driveIter [] db = do
  putStrLn $ "driveIter: all done with database size " ++ (show . length) db
  return db

-- TODO: only launch if we haven't already launched.
driveIter ((L query):ns) db = do
  putStrLn "driveIter: Launching a query"
  ns' <- runAction (launch query)
  putStrLn $ "driveIter: Action produced "
          ++ (show . length) ns'
          ++ " addition nexts."
  driveIter (ns' ++ ns) db
  -- the ordering of this concatenation is going to
  -- have some influence on whether we behave vaguely
  -- breadth first or depth first.

driveIter (y@(Y query answer):ns) db = do
  putStrLn "driveIter: TODO: yield/Y not fully implemented"

  -- what is yield going to do?
  -- two things:
  -- 1. add the current answer to the answer database for
  --    future iterations
  -- and 
  -- 2. execute any callbacks that are waiting on the
  --    results of this query, in the callback database.
  -- To begin with, 1 is the one I want to deal with.

  let db' = updateDBWithYield db query answer
  driveIter ns db'

data Next where
  -- | yield a result for a query
  Y :: Query q => q -> Answer q -> Next

  -- | launch a query
  L :: Query q => q -> Next

-- | This is not parameterised by the type of query,
--   so that a collecton of DBEntries can represent
--   different types of queries.
--   An entry will contain both the answers for this
--   query already encountered, and (TODO) callbacks to be
--   run for new answers.
data DBEntry where
  DBEntry :: Query q => q -> [Answer q] -> DBEntry

updateDBWithYield :: Query q => [DBEntry] -> q -> Answer q -> [DBEntry]
updateDBWithYield db query answer = db ++ [DBEntry query [answer]] -- TODO needs to update existing entries...

instance Show DBEntry where
  show (DBEntry query anss) =
       "DBEntry { q = "
    ++ show query
    ++ ", n_ans = " ++ (show . length) anss
    ++ ", ans = " ++ show anss
    ++ "}"

