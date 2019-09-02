{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ExplicitForAll #-}
{-# language AllowAmbiguousTypes #-}

module Data.Arithmetic.Plus
  ( zero
  , commutative
  , associative
  ) where

import Data.Arithmetic.Unsafe (type (:=:)(Equal))
import GHC.TypeNats (type (+))
import qualified GHC.TypeNats as GHC

zero :: m :=: (m + 0)
zero = Equal

commutative :: forall a b. a + b :=: b + a
commutative = Equal

associative :: forall a b c. (a + b) + c :=: a + (b + c)
associative = Equal
