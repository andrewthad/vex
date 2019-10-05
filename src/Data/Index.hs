{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Data.Index
  ( Index(..)
  , incrementL
  , incrementR
  , incrementLimitR
  , incrementLimitL
    -- * Functions
  , ascendM
  , ascendM1
    -- * Constants
  , i01
  ) where

import Arithmetic.Types (type (<),Nat)
import Data.Kind (Type)
import GHC.TypeNats (type (+))
import Arithmetic.Nat ((<?))
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified GHC.TypeNats as GHC

data Index :: GHC.Nat -> Type where
  Index :: (m < n) -> Nat m -> Index n

incrementL :: forall m n. Nat m -> Index n -> Index (m + n)
incrementL x (Index lt y) = Index (Lt.incrementL @m lt) (Nat.plus x y)

incrementR :: forall m n. Nat m -> Index n -> Index (n + m)
incrementR x (Index lt y) = Index (Lt.incrementR @m lt) (Nat.plus y x)

-- | This looks a lot like 'increment', but it does not increase
-- the actual index, only the upper bound.
incrementLimitL :: forall m n. Nat m -> Index n -> Index (m + n)
incrementLimitL _ (Index lt y) = Index (Lt.weakenL @m lt) y

incrementLimitR :: forall m n. Nat m -> Index n -> Index (n + m)
incrementLimitR _ (Index lt y) = Index (Lt.plus lt (Lte.zero @m)) y

i01 :: Index 1
i01 = Index Lt.zero Nat.zero

-- | A strict left monadic fold over the ascending indices from zero up to
-- a given length.
ascendM :: forall m n a. (Monoid a, Monad m) => (Index n -> m a) -> Nat n -> m a
{-# INLINE ascendM #-}
ascendM f n = go Nat.zero mempty
  where
  go :: Nat p -> a -> m a
  go !ix !a = case ix <? n of
    Just lt -> do
      b <- f (Index lt ix)
      go (Nat.succ ix) (a <> b)
    Nothing -> pure a

-- | A strict left monadic fold over the ascending indices from zero up to
-- a given length.
ascendM1 :: forall m n a. (Semigroup a, Monad m) => (Index n -> m a) -> Nat n -> (0 < n) -> m a
{-# INLINE ascendM1 #-}
ascendM1 f n !gt = go Nat.one =<< f (Index gt Nat.zero) 
  where
  go :: Nat p -> a -> m a
  go !ix !a = case ix <? n of
    Just lt -> do
      b <- f (Index lt ix)
      go (Nat.succ ix) (a <> b)
    Nothing -> pure a
