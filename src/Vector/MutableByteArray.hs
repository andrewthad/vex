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

-- | Length-index arrays of 'MutableByteArray'.
module Vector.MutableByteArray
  ( -- Types
    MutableVector
    -- Create
  , uninitialized
    -- Index
  , read
  , write
  ) where

import Prelude hiding (read,length)

import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt))
import Data.Kind (Type)
import Data.Primitive (MutableByteArray(..))
import GHC.Exts (Int(I#))
import GHC.ST (ST(ST))

import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC

data MutableVector :: Type -> GHC.Nat -> Type where
  MutableVector :: Exts.MutableArrayArray# s -> MutableVector s n

-- | Create an array of uninitialized memory. This is
-- exceptionally unsafe when working with a vector of @MutableByteArray@
-- since reading from any index before writing to it will lead to
-- a segfault.
uninitialized :: Nat n -> ST s (MutableVector s n)
{-# inline uninitialized #-}
uninitialized (Nat (I# sz)) = ST
  ( \s0 -> case Exts.newArrayArray# sz s0 of
    (# s1, arr #) -> (# s1, MutableVector arr #)
  )

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
