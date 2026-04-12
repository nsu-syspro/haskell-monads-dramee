{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Data.Functor.Identity

class Applicative m => KleisliMonad m where
  {-# MINIMAL (>=>) #-}
  infixr 1 >=>
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

infixl 1 >>=
(>>=) :: KleisliMonad m => m a -> (a -> m b) -> m b
(>>=) = error "TODO: define (>>=) in Task2"

join :: KleisliMonad m => m (m a) -> m a
join = error "TODO: define join in Task2"

instance KleisliMonad Identity where
  (>=>) :: (a -> Identity b) -> (b -> Identity c) -> (a -> Identity c)
  (>=>) = error "TODO: define (>=>) (KleisliMonad Identity)"

instance KleisliMonad Maybe where
  (>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
  (>=>) = error "TODO: define (>=>) (KleisliMonad Maybe)"

instance KleisliMonad [] where
  (>=>) :: (a -> [b]) -> (b -> [c]) -> (a -> [c])
  (>=>) = error "TODO: define (>=>) (KleisliMonad [])"

instance (Monoid e) => KleisliMonad ((,) e) where
  (>=>) :: Monoid e => (a -> (e, b)) -> (b -> (e, c)) -> (a -> (e, c))
  (>=>) = error "TODO: define (>=>) (KleisliMonad ((,) e))"

instance KleisliMonad ((->) e) where
  (>=>) :: (a -> e -> b) -> (b -> e -> c) -> (a -> e -> c)
  (>=>) = error "TODO: define (>=>) (KleisliMonad ((->) e))"
