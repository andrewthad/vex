{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ExplicitForAll #-}

module Data.Arithmetic.Lt
  ( plus
  , plusR
  , transitive
  , zero
  , substituteR
  , incrementR
  , incrementL
  ) where

import Data.Arithmetic.Unsafe (type (<)(Lt))
import Data.Arithmetic.Unsafe (type (:=:)(Equal))
import GHC.TypeNats (type (+))
import qualified GHC.TypeNats as GHC

substituteR :: (b :=: c) -> (a < b) -> (a < c)
substituteR Equal Lt = Lt

plus :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) -> (c + a < c + b)
plus Lt = Lt

plusR :: forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) -> (a + c < b + c)
plusR Lt = Lt

transitive :: (a < b) -> (b < c) -> (a < c)
transitive Lt Lt = Lt

incrementL :: forall c a b. (a < b) -> (a < c + b)
incrementL Lt = Lt

incrementR :: forall c a b. (a < b) -> (a < b + c)
incrementR Lt = Lt

-- TODO: change this
zero :: forall (a :: GHC.Nat). 0 < a + 1
zero = Lt
