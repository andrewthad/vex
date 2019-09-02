module Data.Nat.Arithmetic
  ( Nat
  , Gt
  , Lt
  ) where

import Prelude hiding (compare)

import qualified GHC.TypeNats as GHC

-- | A value-level representation of a natural number @n@.
newtype Nat (n :: GHC.Nat) = Nat { getNat :: Int }
  deriving (Show,Prim)
type role Length nominal

data Gt :: GHC.Nat -> GHC.Nat -> Type where
  Gt :: Gt a b

