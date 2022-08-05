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
module Vector.MutableByteArray
  ( -- Types
    MutableVector
    -- Create
  , uninitialized
    -- Copy
  , duplicate
    -- Index
  , read
  , write
  ) where

import Prelude hiding (read,length)

import Vector.MutableByteArray.Internal (read,write)
import Vector.MutableByteArray.Types (MutableVector(..))

import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt))
import Data.Kind (Type)
import Data.Primitive (MutableByteArray(..))
import GHC.Exts (Int(I#))
import GHC.ST (ST(ST))

import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC

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

-- | Make a copy of the mutable vector.
duplicate :: Nat n -> MutableVector s n -> ST s (MutableVector s n)
duplicate (Nat (I# n)) (MutableVector arr) = ST
  (\s0 -> case Exts.newArrayArray# n s0 of
    (# s1, dst #) -> case Exts.copyMutableArrayArray# arr 0# dst 0# n s1 of
      s2 -> (# s2, MutableVector arr #)
  )
