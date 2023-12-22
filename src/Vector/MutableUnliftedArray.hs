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

-- | Length-index arrays of 'MutableByteArray'.
module Vector.MutableUnliftedArray
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
import GHC.Exts (Int(I#))
import GHC.ST (ST(ST))
import Data.Primitive.Unlifted.Array (MutableUnliftedArray(..))

import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC

data MutableVector :: Type -> GHC.Nat -> Type -> Type where
  MutableVector :: Exts.MutableArrayArray# s -> MutableVector s n a

-- | Create an array of uninitialized memory. This is
-- exceptionally unsafe when working with a vector of @MutableByteArray@
-- since reading from any index before writing to it will lead to
-- a segfault.
uninitialized :: Nat n -> ST s (MutableVector s n a)
{-# inline uninitialized #-}
uninitialized (Nat (I# sz)) = ST
  ( \s0 -> case Exts.newArrayArray# sz s0 of
    (# s1, arr #) -> (# s1, MutableVector arr #)
  )

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n a -- ^ Array
  -> Nat m -- ^ Index
  -> ST s (MutableUnliftedArray s a)
{-# INLINE read #-}
-- this is a core operation
read Lt (MutableVector arr) (Nat (I# i)) = ST
  (\s0 -> case Exts.readMutableArrayArrayArray# arr i s0 of
    (# s1, r #) -> (# s1, MutableUnliftedArray r #)
  )

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n a -- ^ Array
  -> Nat m -- ^ Index
  -> MutableUnliftedArray s a
  -> ST s ()
{-# INLINE write #-}
-- this is a core operation
write Lt (MutableVector arr) (Nat (I# i)) (MutableUnliftedArray x) = ST
  (\s0 -> (# Exts.writeMutableArrayArrayArray# arr i x s0, () #))
