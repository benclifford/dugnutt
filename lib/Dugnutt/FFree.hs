{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Dugnutt.FFree where

-- | from okmij "Freer monads, More Extensible Effects",
--   I've taken the freer monad stuff, but not the extensible
--   effects stuff (mostly because I didn't want to spend
--   time figuring out if I could get the Query/Answer
--   behaviour to work as an extensible effect).

data FFree f a where
  Pure :: a -> FFree f a
  Impure :: f x -> (x -> FFree f a) -> FFree f a

-- all freers have these:
instance Functor (FFree f) where
  fmap f (Pure v) = Pure (f v)
  fmap f (Impure a b) = Impure a (\x -> fmap f (b x))

instance Applicative (FFree f) where
  pure v = Pure v

  -- | This is potentially parallelisable in a Dugnutt
  --   setting (as happens with Haxl for example). Nothing
  --   uses it at the moment so don't spend time on it.
  --   see for example https://elvishjerricco.github.io/2016/04/08/applicative-effects-in-free-monads.html
  _a <*> _b = error "<*> not implemented for FFree"

instance Monad (FFree f) where
  Pure v >>= rest = rest v
  (Impure cmd k') >>= k = Impure cmd (\v -> ((k' v) >>= k))

instance MonadFail (FFree f) where
  fail str = error ("MonadFail not implemented for FFree: " ++ str)
