{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language GADTSyntax #-}
{-# language ExplicitNamespaces #-}
{-# language TypeApplications #-}

module Data.Array.Char
  ( Vector
  , MutableVector
  , CharVector
  , MutableCharVector
    -- Functions
  , singleton
  , index
  , read
  , write
  , append
  , copy
  , new
  , length
  ) where

import Prelude hiding (read,length)

import Data.Primitive (PrimArray,MutablePrimArray)
import Data.Kind (Type)
import Control.Monad.ST (ST,runST)
import Data.Arithmetic.Unsafe (Nat(Nat),type (<)(Lt), type (<=))
import Control.Monad.ST.Run (runPrimArrayST)
import GHC.TypeNats (type (+))

import qualified Data.Arithmetic.Plus as Plus
import qualified Data.Arithmetic.Equal as Equal
import qualified Data.Arithmetic.Lt as Lt
import qualified Data.Primitive as PM
import qualified GHC.TypeNats as GHC
import qualified Data.Arithmetic.Nat as Nat

type Vector = CharVector
type MutableVector = MutableCharVector

newtype CharVector :: GHC.Nat -> Type where
  CharVector :: PrimArray Char -> CharVector n

newtype MutableCharVector :: Type -> GHC.Nat -> Type where
  MutableCharVector :: MutablePrimArray s Char -> MutableCharVector s n

new :: Nat n -> ST s (MutableVector s n)
{-# INLINE new #-}
-- This is a core operation. Be careful though because the memory
-- is uninitialized.
new (Nat n) = fmap MutableCharVector (PM.newPrimArray n)

singleton :: Char -> Vector 1
{-# INLINE singleton #-}
-- this is a core operation, but it is possible to make it not so
singleton x = CharVector $ runPrimArrayST $ do
  arr <- PM.newPrimArray 1
  PM.writePrimArray arr 0 x
  PM.unsafeFreezePrimArray arr

index ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Vector n -- ^ Array
  -> Nat m -- ^ Index
  -> Char
{-# INLINE index #-}
-- this is a core operation
index Lt (CharVector arr) (Nat i) = PM.indexPrimArray arr i

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> ST s Char
{-# INLINE read #-}
-- this is a core operation
read Lt (MutableCharVector arr) (Nat i) = PM.readPrimArray arr i

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> Char
  -> ST s ()
{-# INLINE write #-}
-- this is a core operation
write Lt (MutableCharVector arr) (Nat i) x = PM.writePrimArray arr i x

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
copy Lt Lt (MutableCharVector dst) (Nat doff) (CharVector src) (Nat soff) (Nat len) =
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
unsafeFreeze (MutableCharVector marr) = do
  fmap CharVector (PM.unsafeFreezePrimArray marr)

length :: Vector n -> Nat n
{-# INLINE length #-}
-- this is a core operation
length (CharVector x) = Nat (PM.sizeofPrimArray x)

