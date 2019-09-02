{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language GADTSyntax #-}
{-# language ExplicitNamespaces #-}
{-# language TypeApplications #-}

module Data.Array.Bool
  ( Vector
  , MutableVector
  , BoolVector
  , MutableBoolVector
    -- Functions
  , singleton
  , index
  , read
  , write
  , append
  , copy
  , new
  , initialize
  , replicate
  , set
  , unsafeFreeze
    -- Substitution
  , substitute
  ) where

import Prelude hiding (read,length,replicate)

import Data.Int (Int8)
import Data.Word (Word8)
import Data.Bool (bool)

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

type Vector = BoolVector
type MutableVector = MutableBoolVector

newtype BoolVector :: GHC.Nat -> Type where
  BoolVector :: PrimArray Int8 -> BoolVector n

newtype MutableBoolVector :: Type -> GHC.Nat -> Type where
  MutableBoolVector :: MutablePrimArray s Int8 -> MutableBoolVector s n

new :: Nat n -> ST s (MutableVector s n)
{-# INLINE new #-}
-- This is a core operation. Be careful though because the memory
-- is uninitialized.
new (Nat n) = fmap MutableBoolVector (PM.newPrimArray n)

initialize :: forall n s. Nat n -> Bool -> ST s (MutableVector s n)
-- Not a core operation.
initialize !n v = do
  m <- new n
  set (Lt.plus @n (Lt.zero @0)) m Nat.zero n v
  pure m

replicate :: Nat n -> Bool -> Vector n
-- Not a core operation.
replicate n v = runST (initialize n v >>= unsafeFreeze)

singleton :: Bool -> Vector 1
{-# INLINE singleton #-}
-- this is a core operation, but it is possible to make it not so
singleton x = BoolVector $ runPrimArrayST $ do
  arr <- PM.newPrimArray 1
  PM.writePrimArray arr 0 (bool 0 1 x :: Int8)
  PM.unsafeFreezePrimArray arr

index ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Vector n -- ^ Array
  -> Nat m -- ^ Index
  -> Bool
{-# INLINE index #-}
-- this is a core operation
index Lt (BoolVector arr) (Nat i) = PM.indexPrimArray arr i == 1

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> ST s Bool
{-# INLINE read #-}
-- this is a core operation
read Lt (MutableBoolVector arr) (Nat i) = do
  x <- PM.readPrimArray arr i
  pure (x == 1)

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> Bool
  -> ST s ()
{-# INLINE write #-}
-- this is a core operation
write Lt (MutableBoolVector arr) (Nat i) x = do
  PM.writePrimArray arr i (bool 0 1 x :: Int8)

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
copy Lt Lt (MutableBoolVector dst) (Nat doff) (BoolVector src) (Nat soff) (Nat len) =
  PM.copyPrimArray dst doff src soff len

set :: 
     (doff + n <= dlen)
  -> MutableVector s dlen -- ^ Destination
  -> Nat doff
  -> Nat n
  -> Bool
  -> ST s ()
-- this is a core operation
set Lt (MutableBoolVector dst) (Nat off) (Nat len) b =
  PM.setPrimArray dst off len (bool 0 1 b :: Int8)

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
unsafeFreeze (MutableBoolVector marr) = do
  fmap BoolVector (PM.unsafeFreezePrimArray marr)

length :: Vector n -> Nat n
{-# INLINE length #-}
-- this is a core operation
length (BoolVector x) = Nat (PM.sizeofPrimArray x)

substitute :: n :=: m -> Vector n -> Vector m
{-# INLINE substitute #-}
-- this is a core operation
substitute Equal (BoolVector x) = BoolVector x
