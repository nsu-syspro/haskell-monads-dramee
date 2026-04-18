{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- Hide built-in bind definition
import Prelude hiding ((>>=))

import Data.Functor.Identity

-- * Kleisli composition monad

-- | Monad based on Kleisli composition '(>=>)' operator
-- instead of usual bind operator '(>>=)'.
class Applicative m => KleisliMonad m where
  infixr 1 >=>
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

-- * Equivalent views

infixl 1 >>=
(>>=) :: KleisliMonad m => m a -> (a -> m b) -> m b
(>>=) ma f = (const ma >=> f) ()



join :: KleisliMonad m => m (m a) -> m a
join = id >=> id

-- * Instances

instance KleisliMonad Identity where
  (>=>) :: (a -> Identity b) -> (b -> Identity c) -> (a -> Identity c)
  (>=>) f g a = g (runIdentity (f a))
instance KleisliMonad Maybe where
  (>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
  (>=>) f g a = g =<< f a
instance KleisliMonad [] where
  (>=>) :: (a -> [b]) -> (b -> [c]) -> (a -> [c])
  (>=>) f g a = concatMap g (f a)

instance (Monoid e) => KleisliMonad ((,) e) where
  (>=>) :: Monoid e => (a -> (e, b)) -> (b -> (e, c)) -> (a -> (e, c))
  (>=>) f g a =
    let (e, b) = f a
        (e', c) = g b
    in (mappend e e', c)

instance KleisliMonad ((->) e) where
  (>=>) :: (a -> e -> b) -> (b -> e -> c) -> (a -> e -> c)
  (>=>) f g a e = g (f a e) e
