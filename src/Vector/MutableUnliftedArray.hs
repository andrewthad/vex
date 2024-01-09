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

-- | Length-index arrays of 'MutableUnliftedArray'.
module Vector.MutableUnliftedArray
  ( -- Types
    MutableVector
  , Vector
    -- Create
  , uninitialized
    -- Index
  , read
  , write
  , index
    -- Freeze
  , unsafeFreeze
  ) where

import Prelude hiding (read,length)

import Data.Primitive.Unlifted.Class (Unlifted)
import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt))
import Data.Kind (Type)
import GHC.Exts (Int(I#),Array#,MutableArray#)
import GHC.ST (ST(ST))
import Data.Primitive.Unlifted.Array (MutableUnliftedArray,MutableUnliftedArray_(MutableUnliftedArray))
import Data.Primitive.Unlifted.Array.Primops (MutableUnliftedArray#(..))

import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC
import qualified Data.Primitive.Unlifted.Array.Primops as Primops

data Vector :: Type -> GHC.Nat -> Type -> Type where
  Vector :: Array# (MutableArray# s (Unlifted a)) -> Vector s n a

data MutableVector :: Type -> GHC.Nat -> Type -> Type where
  MutableVector :: MutableArray# s (MutableArray# s (Unlifted a)) -> MutableVector s n a

-- | Create an array of uninitialized memory. This is
-- exceptionally unsafe when working with a vector of @MutableByteArray@
-- since reading from any index before writing to it will lead to
-- a segfault.
uninitialized :: Nat n -> ST s (MutableVector s n a)
{-# inline uninitialized #-}
uninitialized (Nat (I# sz)) = ST
  ( \s0 -> case Primops.unsafeNewUnliftedArray# sz s0 of
    (# s1, MutableUnliftedArray# arr #) -> (# s1, MutableVector arr #)
  )

index ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Vector s n a -- ^ Array
  -> Nat m -- ^ Index
  -> MutableUnliftedArray s a
{-# inline index #-}
index Lt (Vector arr) (Nat (I# i)) = case Exts.indexArray# arr i of
  (# r #) -> MutableUnliftedArray (MutableUnliftedArray# r)

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n a -- ^ Array
  -> Nat m -- ^ Index
  -> ST s (MutableUnliftedArray s a)
{-# INLINE read #-}
-- this is a core operation
read Lt (MutableVector arr) (Nat (I# i)) = ST
  (\s0 -> case Exts.readArray# arr i s0 of
    (# s1, r #) -> (# s1, MutableUnliftedArray (MutableUnliftedArray# r) #)
  )

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n a -- ^ Array
  -> Nat m -- ^ Index
  -> MutableUnliftedArray s a
  -> ST s ()
{-# INLINE write #-}
-- this is a core operation
write Lt (MutableVector arr) (Nat (I# i)) (MutableUnliftedArray (MutableUnliftedArray# x)) = ST
  (\s0 -> (# Exts.writeArray# arr i x s0, () #))

unsafeFreeze ::
     MutableVector s n a
  -> ST s (Vector s n a)
{-# inline unsafeFreeze #-}
unsafeFreeze (MutableVector marr) = ST
  (\s0 -> case Exts.unsafeFreezeArray# marr s0 of
    (# s1, arr #) -> (# s1, Vector arr #)
  )
