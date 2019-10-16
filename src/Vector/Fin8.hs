{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

-- The only operatations defined in this module are those
-- that are considered primitive. That is, they cannot be
-- defined in terms of other operations on length-indexed
-- vectors.
module Vector.Fin8
  ( -- Types
    Vector
  , MutableVector
    -- Create
  , uninitialized
    -- Weaken
  , weakenMutable
    -- Index
  , index
  , read
  , write
  ) where

import Prelude hiding (read)

import GHC.Exts (Int(I#),(*#))
import GHC.ST (ST(ST))
import Data.Kind (Type)
import GHC.TypeNats (type (+))
import Foreign.Storable (sizeOf)
import Arithmetic.Types (Fin(Fin))
import Arithmetic.Unsafe (type (<=)(Lte))
import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt),type (:=:)(Eq))

import qualified GHC.TypeNats as GHC
import qualified GHC.Exts as Exts

data Vector :: GHC.Nat -> GHC.Nat -> Type where
  Vector :: Exts.ByteArray# -> Vector b n

data MutableVector :: Type -> GHC.Nat -> GHC.Nat -> Type where
  MutableVector :: Exts.MutableByteArray# s -> MutableVector s b n

-- | Create an array of uninitialized memory. This is
-- exceptionally unsafe when working with a vector of @Fin@
-- since the elements must all be treated as invalid. That
-- is, users must not read from an index until they have
-- written to it, but this is not enforced by the type system.
uninitialized :: (b < 256) -> Nat n -> ST s (MutableVector s b n)
{-# inline uninitialized #-}
uninitialized Lt (Nat (I# sz)) = ST
  ( \s0 -> case Exts.newByteArray# sz s0 of
    (# s1, arr #) -> (# s1, MutableVector arr #)
  )

-- | Weaken the bound on an array of finite naturals. The argument
-- array must not be reused after being passed to this function.
weakenMutable :: 
     (c < 256)
  -> (b <= c) -- ^ Evidence that the new bound exceeds the old bound
  -> MutableVector s b n -- ^ Argument must not be reused
  -> MutableVector s c n
weakenMutable Lt Lte (MutableVector v) = MutableVector v

index ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Vector b n -- ^ Array
  -> Nat m -- ^ Index
  -> Fin b
{-# inline index #-}
index Lt (Vector arr) (Nat (I# i)) =
  Fin (Nat (I# (Exts.word2Int# (Exts.indexWord8Array# arr i)))) Lt

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s b n -- ^ Array
  -> Nat m -- ^ Index
  -> ST s (Fin b)
{-# inline read #-}
read Lt (MutableVector arr) (Nat (I# i)) = ST
  (\s0 -> case Exts.readWord8Array# arr i s0 of
    (# s1, val #) -> (# s1, Fin (Nat (I# (Exts.word2Int# val))) Lt #)
  )

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s b n -- ^ Array
  -> Nat m -- ^ Index
  -> Fin b
  -> ST s ()
{-# inline write #-}
write Lt (MutableVector arr) (Nat (I# i)) (Fin (Nat (I# e)) Lt) = ST
  (\s0 -> case Exts.writeWord8Array# arr i (Exts.int2Word# e) s0 of
    s1 -> (# s1, () #)
  )
