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

module Vector.MutableByteArray.Types
  ( MutableVector(..)
  ) where

import Data.Kind (Type)

import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC

data MutableVector :: Type -> GHC.Nat -> Type where
  MutableVector :: Exts.MutableArrayArray# s -> MutableVector s n
