{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language GADTSyntax #-}
{-# language ExplicitNamespaces #-}
{-# language TypeApplications #-}

module Data.Array.DoublePair
  ( Vector
  , MutableVector
  , DoublePairVector
  , MutableDoublePairVector
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

import Data.Tuple.Types (DoublePair)
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

type Vector = DoublePairVector
type MutableVector = MutableDoublePairVector

newtype DoublePairVector :: GHC.Nat -> Type where
  DoublePairVector :: PrimArray DoublePair -> DoublePairVector n

newtype MutableDoublePairVector :: Type -> GHC.Nat -> Type where
  MutableDoublePairVector :: MutablePrimArray s DoublePair -> MutableDoublePairVector s n

new :: Nat n -> ST s (MutableVector s n)
{-# INLINE new #-}
-- This is a core operation. Be careful though because the memory
-- is uninitialized.
new (Nat n) = fmap MutableDoublePairVector (PM.newPrimArray n)

singleton :: DoublePair -> Vector 1
{-# INLINE singleton #-}
-- this is a core operation, but it is possible to make it not so
singleton x = DoublePairVector $ runPrimArrayST $ do
  arr <- PM.newPrimArray 2
  PM.writePrimArray arr 0 x
  PM.unsafeFreezePrimArray arr

index ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Vector n -- ^ Array
  -> Nat m -- ^ Index
  -> DoublePair
{-# INLINE index #-}
-- this is a core operation
index Lt (DoublePairVector arr) (Nat i) = PM.indexPrimArray arr i

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> ST s DoublePair
{-# INLINE read #-}
-- this is a core operation
read Lt (MutableDoublePairVector arr) (Nat i) = PM.readPrimArray arr i

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n -- ^ Array
  -> Nat m -- ^ Index
  -> DoublePair
  -> ST s ()
{-# INLINE write #-}
-- this is a core operation
write Lt (MutableDoublePairVector arr) (Nat i) x = PM.writePrimArray arr i x

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
copy Lt Lt (MutableDoublePairVector dst) (Nat doff) (DoublePairVector src) (Nat soff) (Nat len) =
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
unsafeFreeze (MutableDoublePairVector marr) = do
  fmap DoublePairVector (PM.unsafeFreezePrimArray marr)

length :: Vector n -> Nat n
{-# INLINE length #-}
-- this is a core operation
length (DoublePairVector x) = Nat (PM.sizeofPrimArray x)
