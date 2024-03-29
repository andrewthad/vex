{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language DerivingStrategies #-}
{-# language ExplicitNamespaces #-}
{-# language GADTSyntax #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

module Vector.Boxed
  ( Vector
  , MutableVector
  , index
  , read
  , write
  , update
  , length
  , copy
  , duplicate
  , generate
  , generateST
  , traverseST
  , map'
  , imap'
  , append
  , zipWith'
  , zipWith3'
  , foldlZipWithM
  , zipM_
  , zip3M_
  , foldrMapZipWith
  , foldrZipWith
  , foldrZipWith3
  , foldrZipWith4
  , foldr
  , foldr'
  , foldMap
  , all
  , any
  , empty
  , singleton
  , doubleton
  , tripleton
  , replicate
  , replicateM
  , initialized
  , fromList
  , fromListN
  , shrink
  , unsafeFreeze
  , thaw
  , new
  , forget
  , with
  , substitute
  , runST
  , unsafeCast
  ) where

import Prelude hiding (read,length,foldr,replicate,foldMap,all,any)

import Vector.Boxed.Internal (read,write,index)
import Vector.Boxed.Types (Vector(..),MutableVector(..))

import Arithmetic.Types (Fin(Fin),type (:=:))
import Arithmetic.Unsafe (Nat(Nat),type (<)(Lt), type (<=)(Lte))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT),runMaybeT)
import Data.Kind (Type)
import Data.Primitive (Array,MutableArray)
import GHC.ST (ST(ST))
import GHC.TypeNats (type (+))

import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Control.Monad.ST as ST
import qualified Data.Primitive as PM
import qualified GHC.TypeNats as GHC
import qualified GHC.Exts as Exts

new ::
     Nat n
  -> ST s (MutableVector s n a)
{-# INLINE new #-}
-- this is not a core operation
new n = replicateM n errorThunk

substitute ::
     (m :=: n)
  -> Vector m a
  -> Vector n a
{-# INLINE substitute #-}
-- this is a core operation
substitute !_ (Vector x) = Vector x

replicateM ::
     Nat n
  -> a
  -> ST s (MutableVector s n a)
{-# INLINE replicateM #-}
-- this is a core operation
replicateM (Nat n) a = fmap MutableVector (PM.newArray n a)

replicate :: Nat n -> a -> Vector n a
{-# inline replicate #-}
replicate n a = runST (replicateM n a >>= unsafeFreeze)

initialized ::
     Nat n
  -> a
  -> ST s (MutableVector s n a)
{-# INLINE initialized #-}
-- this is a core operation
initialized = replicateM

empty :: Vector 0 a
{-# NOINLINE empty #-}
empty = runST (replicateM Nat.zero errorThunk >>= unsafeFreeze)

singleton ::
     a
  -> Vector 1 a
{-# INLINE singleton #-}
singleton x = runST (replicateM Nat.one x >>= unsafeFreeze)

doubleton ::
     a
  -> a
  -> Vector 2 a
{-# INLINE doubleton #-}
doubleton x y = runST $ do
  v <- replicateM (Nat.constant @2) x
  write Lt.constant v (Nat.constant @1) y
  unsafeFreeze v

tripleton ::
     a
  -> a
  -> a
  -> Vector 3 a
{-# INLINE tripleton #-}
tripleton x y z = runST $ do
  v <- replicateM (Nat.constant @3) x
  write Lt.constant v (Nat.constant @1) y
  write Lt.constant v (Nat.constant @2) z
  unsafeFreeze v

length :: Vector n a -> Nat n
{-# INLINE length #-}
-- this is a core operation
length (Vector x) = Nat (PM.sizeofArray x)

-- | Make a copy of the mutable vector.
duplicate :: Nat n -> MutableVector s n a -> ST s (MutableVector s n a)
{-# INLINE duplicate #-}
duplicate (Nat n) (MutableVector arr) = do
  dst <- PM.cloneMutableArray arr 0 n
  pure (MutableVector dst)

-- | Shrink the argument vector, possibly in-place.
-- The argument vector must not be reused after being
-- passed to this function.
shrink ::
     (m <= n)
  -> Nat m
  -> MutableVector s n a -- ^ Vector to shrink
  -> ST s (MutableVector s m a)
-- this is a core operation
{-# INLINE shrink #-}
shrink Lte (Nat sz) (MutableVector x) =
  if PM.sizeofMutableArray x == sz
    then pure (MutableVector x)
    else fmap MutableVector (PM.cloneMutableArray x 0 sz)

-- | Freeze the mutable vector. The argument must not be reused after
-- this function is called on it. 
unsafeFreeze ::
     MutableVector s n a
  -> ST s (Vector n a)
{-# INLINE unsafeFreeze #-}
-- this is a core operation
unsafeFreeze (MutableVector marr) = do
  fmap Vector (PM.unsafeFreezeArray marr)

copy ::
     (doff + n <= dlen)
  -> (soff + n <= slen)
  -> MutableVector s dlen a -- ^ Destination
  -> Nat doff
  -> Vector slen a -- ^ Source
  -> Nat soff
  -> Nat n
  -> ST s ()
-- this is a core operation
{-# inline copy #-}
copy Lte Lte (MutableVector dst) (Nat doff) (Vector src) (Nat soff) (Nat len) =
  PM.copyArray dst doff src soff len

thaw :: Nat n -> Vector n a -> ST s (MutableVector s n a)
{-# inline thaw #-}
thaw (Nat n) (Vector x) = do
  y <- PM.thawArray x 0 n
  pure (MutableVector y)

-- | Monoidal accumulation backed by a lazy right fold.
foldMap :: Monoid m => (a -> m) -> Nat n -> Vector n a -> m
{-# inline foldMap #-}
foldMap f = foldr (\x acc -> f x <> acc) mempty

all :: (a -> Bool) -> Nat n -> Vector n a -> Bool
{-# inline all #-}
all f !n v = foldr (\a acc -> f a && acc) True n v

any :: (a -> Bool) -> Nat n -> Vector n a -> Bool
{-# inline any #-}
any f !n v = foldr (\a acc -> f a || acc) False n v

foldr :: (a -> b -> b) -> b -> Nat n -> Vector n a -> b
{-# inline foldr #-}
foldr f b0 n v = Fin.descend n b0 (\(Fin ix lt) b -> f (index lt v ix) b)

foldr' :: (a -> b -> b) -> b -> Nat n -> Vector n a -> b
{-# inline foldr' #-}
foldr' f b0 n v = Fin.descend' n b0 (\(Fin ix lt) b -> f (index lt v ix) b)

generate :: Nat n -> (Fin n -> a) -> Vector n a
{-# inline generate #-}
generate !n f = runST $ do
  marr <- replicateM n errorThunk
  Fin.ascendM_ n
    (\(Fin ix lt) -> do
      write lt marr ix (f (Fin ix lt))
    )
  unsafeFreeze marr

generateST :: Nat n -> (Fin n -> ST s a) -> ST s (Vector n a)
{-# inline generateST #-}
generateST !n f = do
  marr <- replicateM n errorThunk
  Fin.ascendM_ n
    (\(Fin ix lt) -> do
      write lt marr ix =<< f (Fin ix lt)
    )
  unsafeFreeze marr

map' :: Nat n -> (a -> b) -> Vector n a -> Vector n b
{-# inline map' #-}
map' !n f v = runST $ do
  marr <- replicateM n errorThunk
  Fin.ascendM_ n
    (\(Fin ix lt) -> do
      write lt marr ix $! f (index lt v ix)
    )
  unsafeFreeze marr

imap' :: Nat n -> (Fin n -> a -> b) -> Vector n a -> Vector n b
{-# inline imap' #-}
imap' !n f v = runST $ do
  marr <- replicateM n errorThunk
  Fin.ascendM_ n
    (\fin@(Fin ix lt) -> do
      write lt marr ix $! f fin (index lt v ix)
    )
  unsafeFreeze marr

traverseST :: Nat n -> (a -> ST s b) -> Vector n a -> ST s (Vector n b)
{-# inline traverseST #-}
traverseST !n f v = do
  marr <- replicateM n errorThunk
  Fin.ascendM_ n
    (\(Fin ix lt) -> do
      write lt marr ix =<< f (index lt v ix)
    )
  unsafeFreeze marr

append :: forall m n a. Vector m a -> Vector n a -> Vector (m + n) a
-- Not a core operation. Defined safely using other primitives.
append x y = runST $ do
  let xlen = length x
  let ylen = length y
  r <- new (Nat.plus xlen ylen)
  copy
    (Lte.incrementL @m (Lte.zero @n))
    Lte.reflexive
    r Nat.zero x Nat.zero xlen
  copy
    Lte.reflexive
    Lte.reflexive
    r xlen y Nat.zero ylen
  unsafeFreeze r

-- | Discard the phantom length associated with an indexed vector.
forget :: Vector n a -> Array a
{-# INLINE forget #-}
-- This is a core operation
forget (Vector arr) = arr

errorThunk :: a
{-# NOINLINE errorThunk #-}
errorThunk = error "Data.Array.Indexed: uninitialized element"

with :: Array a -> (forall n. Vector n a -> b) -> b
{-# inline with #-}
with x f = f (Vector x)

runST :: (forall s. ST s (Vector n a)) -> Vector n a
{-# inline runST #-}
runST f = Vector (PM.Array (Exts.runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, Vector (PM.Array r) #) -> r }})))

unsafeCast :: Array a -> Vector n a
{-# inline unsafeCast #-}
unsafeCast = Vector

update ::
     (m < n) -- ^ Evidence the index is in-bounds
  -> Nat n -- ^ Array length
  -> Vector n a -- ^ Array
  -> Nat m -- ^ Index
  -> a -- ^ new value
  -> Vector n a
{-# INLINE update #-}
update lt n v ix a = runST $ do
  v' <- thaw n v
  write lt v' ix a
  unsafeFreeze v'

zipWith' ::
     (a -> b -> c)
  -> Nat n
  -> Vector n a
  -> Vector n b
  -> Vector n c
{-# inline zipWith' #-}
zipWith' f !n !as !bs = runST $ do
  dst <- initialized n errorThunk
  Fin.ascendM_ n $ \(Fin ix lt) -> do
    let !z = f (index lt as ix) (index lt bs ix)
    write lt dst ix z
  unsafeFreeze dst

zipWith3' ::
     (a -> b -> c -> d)
  -> Nat n
  -> Vector n a
  -> Vector n b
  -> Vector n c
  -> Vector n d
{-# inline zipWith3' #-}
zipWith3' f !n !as !bs !cs = runST $ do
  dst <- initialized n errorThunk
  Fin.ascendM_ n $ \(Fin ix lt) -> do
    let !z = f (index lt as ix) (index lt bs ix) (index lt cs ix)
    write lt dst ix z
  unsafeFreeze dst

foldlZipWithM :: Monad m
  => (c -> a -> b -> m c)
  -> c
  -> Nat n
  -> Vector n a
  -> Vector n b
  -> m c
{-# inline foldlZipWithM #-}
foldlZipWithM f z !n !as !bs = Fin.ascendM n z $ \(Fin ix lt) acc ->
  f acc (index lt as ix) (index lt bs ix)

zipM_ :: Monad m
  => (a -> b -> m c)
  -> Nat n
  -> Vector n a
  -> Vector n b
  -> m ()
{-# inline zipM_ #-}
zipM_ f !n !as !bs = do
  Fin.ascendM_ n $ \(Fin ix lt) ->
    f (index lt as ix) (index lt bs ix)
  pure ()

zip3M_ :: Monad m
  => (a -> b -> c -> m d)
  -> Nat n
  -> Vector n a
  -> Vector n b
  -> Vector n c
  -> m ()
{-# inline zip3M_ #-}
zip3M_ f !n !as !bs !cs = do
  Fin.ascendM_ n $ \(Fin ix lt) ->
    f (index lt as ix) (index lt bs ix) (index lt cs ix)
  pure ()

-- | Lazy right fold
foldrMapZipWith :: Monoid m
  => (a -> b -> m)
  -> Nat n
  -> Vector n a
  -> Vector n b
  -> m
{-# inline foldrMapZipWith #-}
foldrMapZipWith f !n !as !bs =
  Fin.descend n mempty (\(Fin ix lt) acc -> f (index lt as ix) (index lt bs ix) <> acc)

-- | Lazy right fold
foldrZipWith ::
     (a -> b -> c -> c)
  -> c
  -> Nat n
  -> Vector n a
  -> Vector n b
  -> c
{-# inline foldrZipWith #-}
foldrZipWith f c0 !n !as !bs =
  Fin.descend n c0 (\(Fin ix lt) acc -> f (index lt as ix) (index lt bs ix) acc)

-- | Lazy right fold over three arrays
foldrZipWith3 ::
     (a -> b -> c -> d -> d)
  -> d
  -> Nat n
  -> Vector n a
  -> Vector n b
  -> Vector n c
  -> d
{-# inline foldrZipWith3 #-}
foldrZipWith3 f d0 !n !as !bs !cs =
  Fin.descend n d0 (\(Fin ix lt) acc -> f (index lt as ix) (index lt bs ix) (index lt cs ix) acc)

-- | Lazy right fold over four arrays
foldrZipWith4 ::
     (a -> b -> c -> d -> e -> e)
  -> e
  -> Nat n
  -> Vector n a
  -> Vector n b
  -> Vector n c
  -> Vector n d
  -> e
{-# inline foldrZipWith4 #-}
foldrZipWith4 f d0 !n !as !bs !cs !ds =
  Fin.descend n d0 (\(Fin ix lt) acc -> f (index lt as ix) (index lt bs ix) (index lt cs ix) (index lt ds ix) acc)

-- | Fails with Nothing if the list is the wrong size
fromListN :: Nat n -> [a] -> Maybe (Vector n a)
fromListN = fromList

-- | Fails with Nothing if the list is the wrong size
fromList :: Nat n -> [a] -> Maybe (Vector n a)
fromList !n xs0 = ST.runST $ do
  dst <- initialized n errorThunk
  m <- runMaybeT $ Fin.ascendM n xs0 $ \(Fin ix lt) xs -> case xs of
    [] -> MaybeT (pure Nothing)
    y : ys -> MaybeT $ do
      write lt dst ix y
      pure (Just ys)
  case m of
    Just remaining -> case remaining of
      [] -> do
        dst' <- unsafeFreeze dst
        pure (Just dst')
      _ -> pure Nothing
    Nothing -> pure Nothing
