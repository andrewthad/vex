{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language DerivingStrategies #-}
{-# language ExplicitNamespaces #-}
{-# language GADTSyntax #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}
module Vector.Boxed.Internal
  ( index
  , read
  , write
  ) where

import Prelude hiding (read,length,foldr,replicate,foldMap,all,any)

import Vector.Boxed.Types

import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt), type (<=)(Lte))
import Control.Monad.ST (ST)

import qualified Data.Primitive as PM

index ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Vector n a -- ^ Array
  -> Nat m -- ^ Index
  -> a
{-# INLINE index #-}
-- this is a core operation
index Lt (Vector arr) (Nat i) = PM.indexArray arr i

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n a -- ^ Array
  -> Nat m -- ^ Index
  -> ST s a
{-# INLINE read #-}
-- this is a core operation
read Lt (MutableVector arr) (Nat i) = PM.readArray arr i

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s n a -- ^ Array
  -> Nat m -- ^ Index
  -> a
  -> ST s ()
{-# INLINE write #-}
-- this is a core operation
write Lt (MutableVector arr) (Nat i) x = PM.writeArray arr i x

