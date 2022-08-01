{-# language DataKinds #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language DerivingStrategies #-}
{-# language GADTSyntax #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}

module Vector.Boxed.Types
  ( Vector(..)
  , MutableVector(..)
  ) where

import Data.Primitive (Array,MutableArray)
import Data.Kind (Type) 

import qualified GHC.TypeNats as GHC

newtype Vector :: GHC.Nat -> Type -> Type where
  Vector :: Array a -> Vector n a
  deriving stock (Functor,Foldable,Traversable)

deriving newtype instance Eq a => Eq (Vector n a)
deriving newtype instance Show a => Show (Vector n a)

newtype MutableVector :: Type -> GHC.Nat -> Type -> Type where
  MutableVector :: MutableArray s a -> MutableVector s n a

