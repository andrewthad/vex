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

-- The only operatations defined in this module are those
-- that are considered primitive. That is, they cannot be
-- defined in terms of other operations on length-indexed
-- vectors.
module Vector.Fin16
  ( -- Types
    Vector
  , MutableVector
    -- Constants
  , empty
    -- Create
  , uninitialized
  , initialized
  , replicate
  , identityM
  , thaw
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
    -- Folds
  , foldlM'
  , traverse_
    -- Conversion
  , expose
  , unsafeCast
  , unsafeCastMutable
  ) where

import Prelude hiding (read,replicate)

import Control.Monad.ST.Run (runByteArrayST)
import Data.Primitive (ByteArray(ByteArray),MutableByteArray(MutableByteArray))
import GHC.Exts (Int(I#),(*#))
import GHC.ST (ST(ST))
import Data.Word (Word16)
import Data.Kind (Type)
import Arithmetic.Types (Fin(Fin))
import Arithmetic.Unsafe (type (<=)(Lte))
import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt),type (:=:)(Eq))

import qualified Arithmetic.Fin as Fin
import qualified Data.Primitive as PM
import qualified GHC.TypeNats as GHC
import qualified GHC.Exts as Exts
import qualified GHC.Prim.Compat as Compat

data Vector :: GHC.Nat -> GHC.Nat -> Type where
  Vector :: Exts.ByteArray# -> Vector b n

data MutableVector :: Type -> GHC.Nat -> GHC.Nat -> Type where
  MutableVector :: Exts.MutableByteArray# s -> MutableVector s b n

-- | The empty vector. It is not necessary to prove anything about
-- the upper bound.
empty :: Vector b 0
empty = Vector (case mempty of {ByteArray a -> a})

-- | Create an array of uninitialized memory. This is
-- exceptionally unsafe when working with a vector of @Fin@
-- since the elements must all be treated as invalid. That
-- is, users must not read from an index until they have
-- written to it, but this is not enforced by the type system.
uninitialized :: forall (b :: GHC.Nat) (n :: GHC.Nat) (s :: Type).
  (b <= 65536) -> Nat n -> ST s (MutableVector s b n)
{-# inline uninitialized #-}
uninitialized Lte (Nat (I# sz)) = ST
  ( \s0 -> case Exts.newByteArray# (sz *# 2# ) s0 of
    (# s1, arr #) -> (# s1, MutableVector arr #)
  )

-- | Create a mutable vector where all elements are initialized to their
-- position in the vector.
identityM :: forall (n :: GHC.Nat) (s :: Type). (n <= 65536) -> Nat n -> ST s (MutableVector s n n)
{-# inline identityM #-}
identityM lte !n = do
  dst <- uninitialized lte n
  Fin.ascendM_ n $ \fin@(Fin ix lt) -> do
    write lt dst ix fin
  pure dst

-- | Make a mutable copy of an immutable vector.
thaw :: Nat n -> Vector b n -> ST s (MutableVector s b n)
{-# inline thaw #-}
thaw (Nat n) (Vector x) = do
  MutableByteArray y <- PM.thawByteArray (ByteArray x) 0 (2 * n)
  pure (MutableVector y)

initialized :: forall (b :: GHC.Nat) (n :: GHC.Nat) (s :: Type).
  (b <= 65536) -> Nat n -> Fin b -> ST s (MutableVector s b n)
{-# inline initialized #-}
initialized _ (Nat sz) (Fin (Nat e) _) = do
  dst <- PM.newByteArray (2 * sz)
  PM.setByteArray dst 0 sz (fromIntegral @Int @Word16 e)
  case dst of
    MutableByteArray x -> pure (MutableVector x)

-- | Create an array in which all elements are the same value.
replicate :: forall (b :: GHC.Nat) (n :: GHC.Nat).
  (b <= 65536) -> Nat n -> Fin b -> Vector b n
{-# inline replicate #-}
replicate _ (Nat sz) (Fin (Nat e) _) =
  let res = runByteArrayST $ do
        dst <- PM.newByteArray (2 * sz)
        PM.setByteArray dst 0 sz (fromIntegral @Int @Word16 e)
        PM.unsafeFreezeByteArray dst
   in case res of
        ByteArray x -> Vector x

-- | Weaken the bound on an array of finite naturals. The argument
-- array must not be reused after being passed to this function.
weakenMutable :: 
     (c <= 65536)
  -> (b <= c) -- ^ Evidence that the new bound exceeds the old bound
  -> MutableVector s b n -- ^ Argument must not be reused
  -> MutableVector s c n
{-# inline weakenMutable #-}
weakenMutable Lte Lte (MutableVector v) = MutableVector v

index ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Vector b n -- ^ Array
  -> Nat m -- ^ Index
  -> Fin b
{-# inline index #-}
index Lt (Vector arr) (Nat (I# i)) =
  Fin (Nat (I# (Exts.word2Int# (Compat.indexWord16Array# arr i)))) Lt

read ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s b n -- ^ Array
  -> Nat m -- ^ Index
  -> ST s (Fin b)
{-# inline read #-}
read Lt (MutableVector arr) (Nat (I# i)) = ST
  (\s0 -> case Compat.readWord16Array# arr i s0 of
    (# s1, val #) -> (# s1, Fin (Nat (I# (Exts.word2Int# val))) Lt #)
  )

write ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> MutableVector s b n -- ^ Array
  -> Nat m -- ^ Index
  -> Fin b
  -> ST s ()
{-# inline write #-}
write Lt (MutableVector arr) (Nat (I# i)) (Fin (Nat (I# e)) Lt) = ST
  (\s0 -> case Compat.writeWord16Array# arr i (Exts.int2Word# e) s0 of
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
  (\s0 -> (# Exts.shrinkMutableByteArray# x (sz *# 2# ) s0, MutableVector x #) )

expose :: Vector b n -> ByteArray
{-# inline expose #-}
expose (Vector x) = ByteArray x

-- | This is really unsafe. The caller must ascertain that
-- all bytes are less than @b@. Do not choose a @b@ greater
-- than 65536.
unsafeCast :: ByteArray -> Vector b n
{-# inline unsafeCast #-}
unsafeCast (ByteArray x) = Vector x

-- | This is really unsafe. See unsafeCast.
unsafeCastMutable :: MutableByteArray s -> MutableVector s b n
{-# inline unsafeCastMutable #-}
unsafeCastMutable (MutableByteArray x) = MutableVector x

substitute :: (m :=: n) -> Vector b m -> Vector b n
{-# inline substitute #-}
substitute Eq (Vector a) = Vector a

substituteBound :: (b :=: c) -> Vector b m -> Vector c n
{-# inline substituteBound #-}
substituteBound Eq (Vector a) = Vector a

equals :: Nat n -> Vector b n -> Vector b n -> Bool
{-# inline equals #-}
equals !(Nat (I# i)) (Vector x) (Vector y) =
  case Exts.compareByteArrays# x 0# y 0# (i *# 2# ) of
    0# -> True
    _ -> False

foldlM' :: Monad m
  => (a -> Fin b -> m a)
  -> a              
  -> Nat n          
  -> Vector b n
  -> m a
{-# inline foldlM' #-}
foldlM' g !a0 !n !v = Fin.ascendM n a0
  (\(Fin ix lt) !a -> g a (index lt v ix))                                  

traverse_ :: Monad m
  => (Fin b -> m a)
  -> Nat n          
  -> Vector b n
  -> m ()
{-# inline traverse_ #-}
traverse_ g !n !v = Fin.ascendM_ n (\(Fin ix lt) -> g (index lt v ix))
