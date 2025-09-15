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
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

module Vector.Optional256
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
  , null
  , union
  , unionWith
  ) where

import Prelude hiding (null)

import Arithmetic.Types (Nat,Fin,type (<))
import Control.Monad.ST (ST)
import Data.Functor.Classes (Eq1(..))
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)

import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Nat as Nat
import qualified Data.IntMap.Strict as IntMap 
import qualified GHC.TypeNats as GHC

newtype Vector :: GHC.Nat -> Type -> Type where
  Vector :: IntMap a -> Vector n a

deriving stock instance Eq a => Eq (Vector n a)
deriving stock instance Functor (Vector n)
deriving stock instance Foldable (Vector n)
deriving stock instance Traversable (Vector n)

instance Eq1 (Vector n) where
  liftEq f (Vector a) (Vector b) = liftEq f a b

index :: (m < n) -> Vector n a -> Nat m -> Maybe a
{-# inline index #-}
index _ (Vector m) n = IntMap.lookup (Nat.demote n) m

-- | Left-biased union of the vectors.
union ::
     Nat n
  -> Vector n a
  -> Vector n a
  -> Vector n a
union _ (Vector x) (Vector y) = Vector (IntMap.union x y)

unionWith ::
     (a -> a -> a)
  -> Nat n
  -> Vector n a
  -> Vector n a
  -> Vector n a
unionWith f _ (Vector x) (Vector y) = Vector (IntMap.unionWith f x y)

null :: Vector n a -> Bool
{-# inline null #-}
null (Vector x) = IntMap.null x

ifoldr :: (Fin n -> a -> b -> b) -> b -> Nat n -> Vector n a -> b
{-# inline ifoldr #-}
ifoldr f b0 !n (Vector m) = IntMap.foldrWithKey
  (\k v acc -> case Fin.fromInt n k of
    Just fin -> f fin v acc
    Nothing -> error "Vector.Optional256.ifoldr: invariant violated"
  ) b0 m

map' :: Nat n -> (a -> b) -> Vector n a -> Vector n b
{-# inline map' #-}
map' _ f (Vector m) = Vector (IntMap.map f m)

imap' :: Nat n -> (Fin n -> a -> b) -> Vector n a -> Vector n b
{-# inline imap' #-}
imap' !n f (Vector m) = Vector $ IntMap.mapWithKey
  (\k v -> case Fin.fromInt n k of
    Just fin -> f fin v
    Nothing -> error "Vector.Optional256.imap': invariant violated"
  ) m

update' ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Nat n -- ^ Array length
  -> Vector n a -- ^ Array
  -> Nat m -- ^ Index
  -> (Maybe a -> a) -- ^ update to create new value
  -> Vector n a
{-# inline update' #-}
update' _ _ (Vector m) ix f = Vector $ IntMap.alter
  (\mold -> Just (f mold)
  ) (Nat.demote ix) m

assign ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Nat n -- ^ Array length
  -> Vector n a -- ^ Array
  -> Nat m -- ^ Index
  -> a -- ^ new value
  -> Vector n a
{-# inline assign #-}
assign _ _ (Vector m) ix a = Vector (IntMap.insert (Nat.demote ix) a m)

nothings :: (n < 128) -> Vector n a
nothings _ = Vector mempty

ifoldMap :: Monoid m => (Fin n -> a -> m) -> Nat n -> Vector n a -> m
ifoldMap f !n (Vector m) = IntMap.foldMapWithKey
  (\k v -> case Fin.fromInt n k of
    Just fin -> f fin v
    Nothing -> error "Vector.Optional256.ifoldMap': invariant violated"
  ) m


traverseST :: Nat n -> (a -> ST s b) -> Vector n a -> ST s (Vector n b)
{-# inline traverseST #-}
traverseST !_ f (Vector m) = Vector <$> IntMap.traverseWithKey (\_ x -> f x) m

itraverse_ :: Monad m => Nat n -> (Fin n -> a -> m b) -> Vector n a -> m ()
{-# inline itraverse_ #-}
itraverse_ !n f (Vector m) = do
  _ <- IntMap.traverseWithKey
    (\k v -> case Fin.fromInt n k of
      Just fin -> f fin v
      Nothing -> error "Vector.Optional256.ifoldMap': invariant violated"
    ) m
  pure ()

