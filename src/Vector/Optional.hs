{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
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

-- This require doing a bunch of unsafe stuff under the hood. The abstraction
-- offered by natural-arithmetic does not work well for this.
module Vector.Optional
  ( Vector
  , index
  , ifoldMap
  , assign
  , nothings
  , update'
  , map'
  , imap'
  , ifoldr
  , itraverse_
  , traverseST
  , traverseIntersection_
  ) where

import Control.Monad.ST (ST,runST)
import Data.Primitive (SmallArray)
import Data.Kind (Type)
import Data.Word (Word64)
import Data.Bits (popCount,unsafeShiftL,testBit,countTrailingZeros,clearBit)
import Data.Bits ((.&.),setBit)
import Arithmetic.Types (Nat,Fin(Fin),type (<))
import Data.Functor.Classes (Eq1(..))

import qualified Arithmetic.Unsafe as Unsafe
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as C
import qualified GHC.TypeNats as GHC

data Vector :: GHC.Nat -> Type -> Type where
  Vector :: -- invariant: popcnt(mask) === length(array)
       !Word64
    -> !(SmallArray a)
    -> Vector n a
  deriving stock (Functor,Foldable,Traversable)

instance Eq1 (Vector n) where
  liftEq f (Vector mask0 vals0) (Vector mask1 vals1) = if mask0 == mask1
    then liftEq f vals0 vals1
    else False

index :: (m < n) -> Vector n a -> Nat m -> Maybe a
{-# inline index #-}
index _ (Vector mask vals) (Unsafe.Nat ix) = case testBit mask ix of
  False  -> Nothing
  True -> case PM.indexSmallArray## vals (logicalToPhysical mask ix) of
    (# r #) -> Just r

ifoldMap :: Monoid m => (Fin n -> a -> m) -> Nat n -> Vector n a -> m
{-# inline ifoldMap #-}
ifoldMap f !_ (Vector mask0 vals) = go 0 mask0 where
  go !physicalIx !mask = case mask of
    0 -> mempty
    _ -> case PM.indexSmallArray## vals physicalIx of
      (# val #) ->
        let logicalIx = countTrailingZeros mask
         in f (Fin (Unsafe.Nat logicalIx) Unsafe.Lt) val
            <>
            go (physicalIx + 1) (clearBit mask logicalIx)

ifoldr :: (Fin n -> a -> b -> b) -> b -> Nat n -> Vector n a -> b
{-# inline ifoldr #-}
ifoldr f b0 !_ (Vector mask0 vals) = go 0 mask0 where
  go !physicalIx !mask = case mask of
    0 -> b0
    _ -> case PM.indexSmallArray## vals physicalIx of
      (# val #) ->
        let logicalIx = countTrailingZeros mask
         in f (Fin (Unsafe.Nat logicalIx) Unsafe.Lt) val (go (physicalIx + 1) (clearBit mask logicalIx))

-- Precondition: bit at logical position is set to True.
logicalToPhysical :: Word64 -> Int -> Int
{-# inline logicalToPhysical #-}
logicalToPhysical mask ix = popCount (unsafeShiftL mask (63 - ix)) - 1

assign ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Nat n -- ^ Array length
  -> Vector n a -- ^ Array
  -> Nat m -- ^ Index
  -> a -- ^ new value
  -> Vector n a
assign _ !_ (Vector mask vals) (Unsafe.Nat ix) a = case testBit mask ix of
  False  ->
    let mask' = setBit mask ix
     in Vector
          mask'
          (C.insertAt vals (logicalToPhysical mask' ix) a)
  True -> Vector mask (C.replaceAt vals (logicalToPhysical mask ix) a)

update' ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Nat n -- ^ Array length
  -> Vector n a -- ^ Array
  -> Nat m -- ^ Index
  -> (Maybe a -> a) -- ^ update to create new value
  -> Vector n a
{-# inline update' #-}
update' _ !_ (Vector mask vals) (Unsafe.Nat ix) f = case testBit mask ix of
  False  ->
    let mask' = setBit mask ix
        !a = f Nothing
     in Vector
          mask'
          (C.insertAt vals (logicalToPhysical mask' ix) a)
  True ->
    let physicalIx = logicalToPhysical mask ix
     in case PM.indexSmallArray## vals physicalIx of
          (# old #) ->
            let new = f (Just old)
             in Vector mask (C.replaceAt vals physicalIx new)

traverseST :: Nat n -> (a -> ST s b) -> Vector n a -> ST s (Vector n b)
{-# inline traverseST #-}
traverseST !_ f (Vector mask vals) = do
  vals' <- C.traverseP f vals
  pure (Vector mask vals')

itraverse_ :: Monad m => Nat n -> (Fin n -> a -> m b) -> Vector n a -> m ()
{-# inline itraverse_ #-}
itraverse_ !_ f (Vector mask0 vals) = go 0 mask0 where
  go !physicalIx !mask = case mask of
    0 -> pure ()
    _ -> case PM.indexSmallArray## vals physicalIx of
      (# val #) ->
        let logicalIx = countTrailingZeros mask
         in do
              _ <- f (Fin (Unsafe.Nat logicalIx) Unsafe.Lt) val
              go (physicalIx + 1) (clearBit mask logicalIx)

nothings :: (n < 64) -> Vector n a
nothings _ = Vector 0 mempty

map' :: Nat n -> (a -> b) -> Vector n a -> Vector n b
{-# inline map' #-}
map' _ f (Vector mask vals) = Vector mask (C.map' f vals)

imap' :: Nat n -> (Fin n -> a -> b) -> Vector n a -> Vector n b
{-# inline imap' #-}
imap' !_ f (Vector mask0 vals) = runST (PM.newSmallArray (PM.sizeofSmallArray vals) imapUninitialized >>= go 0 mask0) where
  go !physicalIx !mask !dst = case mask of
    0 -> do
      dst' <- PM.unsafeFreezeSmallArray dst
      pure (Vector mask0 dst')
    _ -> case PM.indexSmallArray## vals physicalIx of
      (# val #) -> do
        let !logicalIx = countTrailingZeros mask
        let !b = f (Fin (Unsafe.Nat logicalIx) Unsafe.Lt) val
        PM.writeSmallArray dst physicalIx b
        go (physicalIx + 1) (clearBit mask logicalIx) dst

imapUninitialized :: a
{-# noinline imapUninitialized #-}
imapUninitialized = errorWithoutStackTrace "Vector.Optional: mistake in imap' implementation"

-- | At any index present in both vectors, perform the action.
traverseIntersection_ :: Applicative m => (a -> b -> m c) -> Nat n -> Vector n a -> Vector n b -> m ()
traverseIntersection_ f !_ (Vector maskA valsA) (Vector maskB valsB) = case maskC of
  0 -> pure ()
  _ -> go maskC
  where
  !maskC = maskA .&. maskB
  go !mask = case mask of
    0 -> pure ()
    _ ->
      let logicalIx = countTrailingZeros mask
          physicalIxA = logicalToPhysical maskA logicalIx
          !(# valA #) = PM.indexSmallArray## valsA physicalIxA
          physicalIxB = logicalToPhysical maskB logicalIx
          !(# valB #) = PM.indexSmallArray## valsB physicalIxB
       in f valA valB *> go (clearBit mask logicalIx)
