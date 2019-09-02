{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language GADTSyntax #-}
{-# language ExplicitNamespaces #-}
{-# language TypeApplications #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingStrategies #-}

module Data.Array.Word64
  ( Vector
  , MutableVector
  , Word64Vector
  , MutableWord64Vector
    -- Functions
  , singleton
  , index
  , read
  , write
  , append
  , copy
  , new
  , length
  , unsafeFreeze
  , set
  , initialize
    -- Substitution
  , substitute
  ) where

import Prelude hiding (read,length)

import Data.Word (Word64)
import Data.Primitive (PrimArray,MutablePrimArray)
import Data.Kind (Type)
import Control.Monad.ST (ST,runST)
import Data.Arithmetic.Unsafe (Nat(Nat),type (<)(Lt), type (<=))
import Data.Arithmetic.Unsafe ((:=:)(Equal))
import Control.Monad.ST.Run (runPrimArrayST)
import GHC.TypeNats (type (+))

import qualified Data.Arithmetic.Plus as Plus
import qualified Data.Arithmetic.Equal as Equal
import qualified Data.Arithmetic.Lt as Lt
import qualified Data.Primitive as PM
import qualified GHC.TypeNats as GHC
import qualified Data.Arithmetic.Nat as Nat

type Vector = Word64Vector
type MutableVector = MutableWord64Vector

newtype Word64Vector :: GHC.Nat -> Type where
  Word64Vector :: PrimArray Word64 -> Word64Vector n
  deriving newtype (Eq)

newtype MutableWord64Vector :: Type -> GHC.Nat -> Type where
  MutableWord64Vector :: MutablePrimArray s Word64 -> MutableWord64Vector s n

new :: Nat n -> ST s (MutableVector s n)
{-# INLINE new #-}
-- This is a core operation. Be careful though because the memory
-- is uninitialized.
new (Nat n) = fmap MutableWord64Vector (PM.newPrimArray n)

initialize :: forall n s. Nat n -> Word64 -> ST s (MutableVector s n)
-- Not a core operation.
initialize !n v = do
  m <- new n
  set (Lt.plus @n (Lt.zero @0)) m Nat.zero n v
  pure m

set :: 
     (doff + n <= dlen)
  -> MutableVector s dlen -- ^ Destination
  -> Nat doff
  -> Nat n
  -> Word64
  -> ST s ()
-- this is a core operation
set Lt (MutableWord64Vector dst) (Nat off) (Nat len) b =
  PM.setPrimArray dst off len b

singleton :: Word64 -> Vector 1
{-# INLINE singleton #-}
-- this is a core operation, but it is possible to make it not so
singleton x = Word64Vector $ runPrimArrayST $ do
  arr <- PM.newPrimArray 1
  PM.writePrimArray arr 0 x
  PM.unsafeFreezePrimArray arr

index ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Vector n -- ^ Array
  -> Nat m -- ^ Index
  -> Word64
{-# INLINE index #-}
-- this is a core operation
index Lt (Word64Vector arr) (Nat i) = PM.indexPrimArray arr i

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> ST s Word64
{-# INLINE read #-}
-- this is a core operation
read Lt (MutableWord64Vector arr) (Nat i) = PM.readPrimArray arr i

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> Word64
  -> ST s ()
{-# INLINE write #-}
-- this is a core operation
write Lt (MutableWord64Vector arr) (Nat i) x = PM.writePrimArray arr i x

copy ::
     (doff + n <= dlen)
  -> (soff + n <= slen)
  -> MutableVector s dlen -- ^ Destination
  -> Nat doff
  -> Vector slen -- ^ Source
  -> Nat soff
  -> Nat n
  -> ST s ()
-- this is a core operation
copy Lt Lt (MutableWord64Vector dst) (Nat doff) (Word64Vector src) (Nat soff) (Nat len) =
  PM.copyPrimArray dst doff src soff len

append :: forall m n. Vector m -> Vector n -> Vector (m + n)
-- Not a core operation. Defined safely using other primitives.
append x y = runST $ do
  let xlen = length x
  let ylen = length y
  r <- new (Nat.plus xlen ylen)
  copy
    ( Lt.substituteR (Equal.symmetric (Plus.associative @m @n @1))
    $ Lt.plus @m (Lt.zero @n)
    )
    (Lt.plus @m (Lt.zero @0))
    r Nat.zero x Nat.zero xlen
  copy
    (Lt.plus @(m + n) (Lt.zero @0))
    (Lt.plus @n (Lt.zero @0))
    r xlen y Nat.zero ylen
  unsafeFreeze r

-- | Freeze the mutable vector. The argument must not be reused after
-- this function is called on it. 
unsafeFreeze ::
     MutableVector s n
  -> ST s (Vector n)
{-# INLINE unsafeFreeze #-}
-- this is a core operation
unsafeFreeze (MutableWord64Vector marr) = do
  fmap Word64Vector (PM.unsafeFreezePrimArray marr)

length :: Vector n -> Nat n
{-# INLINE length #-}
-- this is a core operation
length (Word64Vector x) = Nat (PM.sizeofPrimArray x)

substitute :: n :=: m -> Vector n -> Vector m
{-# INLINE substitute #-}
-- this is a core operation
substitute Equal (Word64Vector x) = Word64Vector x

