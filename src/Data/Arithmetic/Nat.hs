{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ExplicitForAll #-}

module Data.Arithmetic.Nat
  ( plus
  , succ
    -- * Compare
  , equals
  , (<?)
    -- * Constants
  , zero
  , one
  ) where

import Prelude hiding (succ)

import Data.Arithmetic.Unsafe (Nat(Nat),type (<)(Lt))
import Data.Arithmetic.Unsafe ((:=:)(Equal))
import GHC.TypeNats (type (+))

(<?) :: Nat a -> Nat b -> Maybe (a < b)
Nat x <? Nat y = if x < y
  then Just Lt
  else Nothing

equals :: Nat a -> Nat b -> Maybe (a :=: b)
equals (Nat x) (Nat y) = if x == y
  then Just Equal
  else Nothing

plus :: Nat a -> Nat b -> Nat (a + b)
plus (Nat x) (Nat y) = Nat (x + y)

succ :: Nat a -> Nat (a + 1)
succ n = plus n one

zero :: Nat 0
zero = Nat 0

one :: Nat 1
one = Nat 1
