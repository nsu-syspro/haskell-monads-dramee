{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Hide built-in bind definition
import Prelude hiding ((>>=))

import Data.Functor.Identity

-- * Join monad

-- | Monad based on 'join' operation
-- instead of usual bind operator '(>>=)'.
class Applicative m => JoinMonad m where
  join :: m (m a) -> m a

-- * Equivalent views

infixl 1 >>=
(>>=) :: JoinMonad m => m a -> (a -> m b) -> m b
(>>=) = error "TODO: define (>>=) in Task1"

infixr 1 >=>
(>=>) :: JoinMonad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = error "TODO: define (>=>) in Task1"

-- * Instances

instance JoinMonad Identity where
  join :: Identity (Identity a) -> Identity a
  join = error "TODO: define join (JoinMonad Identity)"

instance JoinMonad Maybe where
  join :: Maybe (Maybe a) -> Maybe a
  join = error "TODO: define join (JoinMonad Maybe)"

instance JoinMonad [] where
  join :: [[a]] -> [a]
  join = error "TODO: define join (JoinMonad [])"

instance (Monoid e) => JoinMonad ((,) e) where
  join :: Monoid e => (e, (e, a)) -> (e, a)
  join = error "TODO: define join (JoinMonad ((,) e))"

instance JoinMonad ((->) e) where
  join :: (e -> (e -> a)) -> (e -> a)
  join = error "TODO: define join (JoinMonad ((->) e))"
