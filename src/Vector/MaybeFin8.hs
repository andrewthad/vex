{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

-- | This is similar to @Vector.Fin8@ except that the elements
-- are of type @Maybe (Fin b)@ where @b <= 255@ rather than
-- @Fin b@ where @b <= 256@. Internally, we use the number 255
-- to signify @Nothing@.
module Vector.MaybeFin8
  ( -- Types
    Vector
  , MutableVector
    -- Create
  , uninitialized
  , initializedNothing
    -- Weaken
  , weakenMutable
  , substitute
  , substituteBound
  , equals
    -- Index
  , index
  , read
  , write
    -- Freeze
  , shrink
  , unsafeFreeze
    -- Conversion
  , expose
  , unsafeCast
  ) where

import Prelude hiding (read)

import Data.Primitive (ByteArray(ByteArray))
import GHC.Exts (Int(I#),(*#))
import GHC.ST (ST(ST))
import Data.Kind (Type)
import GHC.TypeNats (type (+))
import Foreign.Storable (sizeOf)
import Arithmetic.Types (Fin(Fin))
import Arithmetic.Unsafe (type (<=)(Lte))
import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt),type (:=:)(Eq))

import qualified GHC.TypeNats as GHC
import qualified GHC.Exts as Exts
import qualified GHC.Prim.Compat as Compat

data Vector :: GHC.Nat -> GHC.Nat -> Type where
  Vector :: Exts.ByteArray# -> Vector b n

data MutableVector :: Type -> GHC.Nat -> GHC.Nat -> Type where
  MutableVector :: Exts.MutableByteArray# s -> MutableVector s b n

-- | Create an array of uninitialized memory. This is
-- exceptionally unsafe when working with a vector of @Fin@
-- since the elements must all be treated as invalid. That
-- is, users must not read from an index until they have
-- written to it, but this is not enforced by the type system.
uninitialized :: forall (b :: GHC.Nat) (n :: GHC.Nat) (s :: Type).
  (b <= 255) -> Nat n -> ST s (MutableVector s b n)
{-# inline uninitialized #-}
uninitialized Lte (Nat (I# sz)) = ST
  ( \s0 -> case Exts.newByteArray# sz s0 of
    (# s1, arr #) -> (# s1, MutableVector arr #)
  )

initializedNothing :: forall (b :: GHC.Nat) (n :: GHC.Nat) (s :: Type).
  (b <= 255) -> Nat n -> ST s (MutableVector s b n)
{-# inline initializedNothing #-}
initializedNothing Lte (Nat (I# sz)) = ST
  ( \s0 -> case Exts.newByteArray# sz s0 of
    (# s1, arr #) -> case Exts.setByteArray# arr 0# sz 255# s1 of
      s2 -> (# s2, MutableVector arr #)
  )

-- | Weaken the bound on an array of finite naturals. The argument
-- array must not be reused after being passed to this function.
weakenMutable :: 
     (c <= 255)
  -> (b <= c) -- ^ Evidence that the new bound exceeds the old bound
  -> MutableVector s b n -- ^ Argument must not be reused
  -> MutableVector s c n
weakenMutable Lte Lte (MutableVector v) = MutableVector v

index ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Vector b n -- ^ Array
  -> Nat m -- ^ Index
  -> Maybe (Fin b)
{-# inline index #-}
index Lt (Vector arr) (Nat (I# i)) =
  case Compat.indexWord8Array# arr i of
    255## -> Nothing
    w -> Just (Fin (Nat (I# (Exts.word2Int# w))) Lt)

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s b n -- ^ Array
  -> Nat m -- ^ Index
  -> ST s (Maybe (Fin b))
{-# inline read #-}
read Lt (MutableVector arr) (Nat (I# i)) = ST
  (\s0 -> case Compat.readWord8Array# arr i s0 of
    (# s1, v #) ->
      (# s1, case v of {255## -> Nothing; _ -> Just (Fin (Nat (I# (Exts.word2Int# v))) Lt)} #)
  )

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s b n -- ^ Array
  -> Nat m -- ^ Index
  -> Maybe (Fin b)
  -> ST s ()
{-# inline write #-}
write Lt (MutableVector arr) (Nat (I# i)) Nothing = ST
  (\s0 -> case Compat.writeWord8Array# arr i 255## s0 of
    s1 -> (# s1, () #)
  )
write Lt (MutableVector arr) (Nat (I# i)) (Just (Fin (Nat (I# e)) Lt)) = ST
  (\s0 -> case Compat.writeWord8Array# arr i (Exts.int2Word# e) s0 of
    s1 -> (# s1, () #)
  )

unsafeFreeze ::
     MutableVector s b n
  -> ST s (Vector b n)
{-# inline unsafeFreeze #-}
unsafeFreeze (MutableVector marr) = ST
  (\s0 -> case Exts.unsafeFreezeByteArray# marr s0 of
    (# s1, arr #) -> (# s1, Vector arr #)
  )

shrink ::
     (m <= n)
  -> Nat m
  -> MutableVector s b n -- ^ Vector to shrink
  -> ST s (MutableVector s b m)
{-# inline shrink #-}
shrink Lte (Nat (I# sz)) (MutableVector x) = ST
  (\s0 -> (# Exts.shrinkMutableByteArray# x sz s0, MutableVector x #) )

expose :: Vector b n -> ByteArray
{-# inline expose #-}
expose (Vector x) = ByteArray x

-- | This is really unsafe. The caller must ascertain that
-- all bytes are less than @b@. Do not choose a @b@ greater
-- than 256.
unsafeCast :: ByteArray -> Vector b n
unsafeCast (ByteArray x) = Vector x

substitute :: (m :=: n) -> Vector b m -> Vector b n
substitute Eq (Vector a) = Vector a

substituteBound :: (b :=: c) -> Vector b m -> Vector c n
substituteBound Eq (Vector a) = Vector a

equals :: Nat n -> Vector b n -> Vector b n -> Bool
equals !(Nat (I# i)) (Vector x) (Vector y) =
  case Exts.compareByteArrays# x 0# y 0# i of
    0# -> True
    _ -> False



