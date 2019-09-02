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
    -- * Constants
  , i01
  ) where

import Data.Arithmetic.Types (type (<),Nat)
import Data.Kind (Type)
import GHC.TypeNats (type (+))
import Data.Arithmetic.Nat ((<?))
import qualified Data.Arithmetic.Nat as Nat
import qualified Data.Arithmetic.Lt as Lt
import qualified GHC.TypeNats as GHC

data Index :: GHC.Nat -> Type where
  Index :: (m < n) -> Nat m -> Index n

incrementL :: forall m n. Nat m -> Index n -> Index (m + n)
incrementL x (Index lt y) = Index (Lt.plus @m lt) (Nat.plus x y)

incrementR :: forall m n. Nat m -> Index n -> Index (n + m)
incrementR x (Index lt y) = Index (Lt.plusR @m lt) (Nat.plus y x)

-- | This looks a lot like 'increment', but it does not increase
-- the actual index, only the upper bound.
incrementLimitL :: forall m n. Nat m -> Index n -> Index (m + n)
incrementLimitL _ (Index lt y) = Index (Lt.incrementL @m lt) y

incrementLimitR :: forall m n. Nat m -> Index n -> Index (n + m)
incrementLimitR _ (Index lt y) = Index (Lt.incrementR @m lt) y

i01 :: Index 1
i01 = Index (Lt.zero @0) Nat.zero

-- | A strict left monadic fold over the ascending indices from zero up to
-- a given length.
ascendM :: forall m n a. (Monoid a, Monad m) => (Index n -> m a) -> Nat n -> m a
{-# INLINE ascendM #-}
ascendM f n = go Nat.zero mempty
  where
  go :: Nat p -> a -> m a
  go !ix !a = case ix <? n of
    Just lt -> f (Index lt ix) >>= go (Nat.succ ix)
    Nothing -> pure a
