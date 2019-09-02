{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ExplicitForAll #-}
{-# language ExplicitNamespaces #-}

module Data.Arithmetic.Gt
  ( plus
  , transitive
  ) where

import Data.Arithmetic.Unsafe (type (>)(Gt))
import GHC.TypeNats (type (+))
import qualified GHC.TypeNats as GHC

plus :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a > b) -> (a + c) > (b + c)
plus Gt = Gt

transitive :: (a > b) -> (b > c) -> (a > c)
transitive Gt Gt = Gt
