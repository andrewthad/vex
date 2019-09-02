{-# language RoleAnnotations #-}
{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language TypeOperators #-}
{-# language ExplicitNamespaces #-}

module Data.Arithmetic.Unsafe
  ( Nat(..)
  , type (>)(Gt)
  , type (<)(Lt)
  , type (:=:)(Equal)
  , type (<=)
  , type (>=)
  ) where

import Prelude hiding ((>=),(<=))
import Data.Kind (Type)
import GHC.TypeNats (type (+))
import qualified GHC.TypeNats as GHC

-- Do not import this module unless you enjoy pain.

infix 4 >
infix 4 <
infix 4 >=
infix 4 <=
infix 4 :=:

-- | A value-level representation of a natural number @n@.
newtype Nat (n :: GHC.Nat) = Nat { getNat :: Int }
type role Nat nominal

data (>) :: GHC.Nat -> GHC.Nat -> Type where
  Gt :: a > b

data (<) :: GHC.Nat -> GHC.Nat -> Type where
  Lt :: a < b

data (:=:) :: GHC.Nat -> GHC.Nat -> Type where
  Equal :: a :=: b

type (<=) a b = a < b + 1
type (>=) a b = a + 1 > b
