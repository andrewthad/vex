{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ExplicitForAll #-}

module Data.Arithmetic.Equal
  ( symmetric
  , plusR
  ) where

import Data.Arithmetic.Unsafe (type (:=:)(Equal))
import GHC.TypeNats (type (+))
import qualified GHC.TypeNats as GHC

symmetric :: (m :=: n) -> (n :=: m)
symmetric Equal = Equal

plusR :: forall c m n. (m :=: n) -> (m + c :=: n + c)
plusR Equal = Equal
