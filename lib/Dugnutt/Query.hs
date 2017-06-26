{-# Language TypeFamilies #-}
{-# Language GADTs #-}
{-# Language FlexibleInstances #-}

{-# Options_GHC -fwarn-incomplete-patterns #-}

-- | This module contains the language for writing Dugnutt queries.


module Dugnutt.Query where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO, liftIO)

class Query q where

  -- | Queries of type q will return answers of type (Answer q)
  type Answer q

  -- | Queries of type q can be run using 'run'.
  run :: q -> Action ()

type Action = FFree DugnuttCommand

-- | The language of Dugnutt
data DugnuttCommand v where

  -- |perform some IO action
  LiftIO :: IO v -> DugnuttCommand v

  -- | Yield an answer to a specified query, which does
  --   not have to be the query that is currently being
  --   executed (if there is such).
  Yield :: Query q => q -> Answer q -> DugnuttCommand ()


-- | from okmij "Freer monads, More Extensible Effects",
--   I've taken the freer monad stuff, but not the extensible
--   effects stuff (mostly because I didn't want to spend
--   time figuring out if I could get the Query/Yield
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


-- runs an Action to completion.
runAction :: Action v -> IO v
runAction (Pure v) = return v
runAction (Impure (LiftIO a) k) = do
  v <- a
  runAction $ k v
runAction (Impure (Yield q a) k) = do
  putStrLn "Yield notimplemented... skipping"
  runAction $ k ()

