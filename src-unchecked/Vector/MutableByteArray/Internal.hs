{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language ExplicitNamespaces #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

module Vector.MutableByteArray.Internal
  ( read
  , write
  ) where

import Prelude hiding (read,length)

import Vector.MutableByteArray.Types (MutableVector(..))

import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt))
import Data.Kind (Type)
import Data.Primitive (MutableByteArray(..))
import GHC.Exts (Int(I#))
import GHC.ST (ST(ST))

import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> ST s (MutableByteArray s)
{-# INLINE read #-}
-- this is a core operation
read Lt (MutableVector arr) (Nat (I# i)) = ST
  (\s0 -> case Exts.readMutableByteArrayArray# arr i s0 of
    (# s1, r #) -> (# s1, MutableByteArray r #)
  )

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> MutableByteArray s
  -> ST s ()
{-# INLINE write #-}
-- this is a core operation
write Lt (MutableVector arr) (Nat (I# i)) (MutableByteArray x) = ST
  (\s0 -> (# Exts.writeMutableByteArrayArray# arr i x s0, () #))

