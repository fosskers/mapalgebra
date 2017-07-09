{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables #-}
-- {-# LANGUAGE Strict #-}  -- Does this improve performance?

-- |
-- Module    : Geography.MapAlgebra
-- Copyright : (c) Colin Woodbury, 2017
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- This library is an implementation of /Map Algebra/ as described in the
-- book /GIS and Cartographic Modeling/ (GaCM) by Dana Tomlin. The fundamental
-- primitive is the `Raster`, a rectangular grid of data that usually describes
-- some area on the earth. A `Raster` need not contain numerical data, however,
-- and need not just represent satellite imagery. It is essentially a matrix,
-- which of course forms a `Functor`, and thus is available for all the
-- operations we would expect to run on any Functor. /GIS and Cartographic Modeling/
-- doesn't lean on this fact, and so describes many seemingly custom
-- operations which to Haskell are just applications of `fmap` or `zipWith`
-- with pure functions.
--
-- Here are the main classes of operations ascribed to /Map Algebra/ and their
-- corresponding approach in Haskell:
--
-- * Single-Raster /Local Operations/ -> `fmap` with pure functions
-- * Multi-Raster /Local Operations/ -> `foldl` with `zipWith` and pure functions
-- * /Focal Operations/ -> Called /convolution/ elsewhere; 'repa' has support for this (described below)
-- * /Zonal Operations/ -> ??? TODO
--
-- Whether it is meaningful to perform operations between two given
-- `Raster`s (i.e. whether the Rasters properly overlap on the earth) is not
-- handled in this library and is left to the application.

{- TODO

Benchmark against numpy as well as GT's collections API
Checkout `repa-io`

-}

module Geography.MapAlgebra
  (
    -- * Types
    Raster(..)
  , constant
  , Projection(..)
  , reproject
  , Sphere, LatLng, WebMercator
  , Point(..)
  -- * Map Algebra
  -- ** Local Operations
  -- | Operations between `Raster`s. If the source Rasters aren't the
  -- same size, the size of the result will be their intersection. All operations
  -- are element-wise:
  --
  -- @
  -- 1 1 + 2 2  ==  3 3
  -- 1 1   2 2      3 3
  --
  -- 2 2 * 3 3  ==  6 6
  -- 2 2   3 3      6 6
  -- @
  --
  -- If an operation you need isn't available here, use our `zipWith`:
  --
  -- @
  -- zipWith :: (a -> b -> d) -> Raster p c r a -> Raster p c r b -> Raster p c r d
  --
  -- -- Your operation, which you should INLINE and use bang patterns with.
  -- foo :: Int -> Int -> Int
  --
  -- bar :: Projection p => Raster p Int -> Raster p Int -> Raster p Int
  -- bar a b = zipWith foo a b
  -- @
  , zipWith
  -- *** Unary
  -- | If you want to do simple unary @Raster -> Raster@ operations (called
  -- /LocalCalculation/ in GaCM), `Raster` is a `Functor` so you can use
  -- `fmap` as normal:
  --
  -- @
  -- myRaster :: Raster p c r Int
  -- abs :: Num a => a -> a
  --
  -- -- Absolute value of all values in the Raster.
  -- fmap abs myRaster
  -- @
  , classify
  -- *** Binary
  -- | You can safely use these with the `foldl` family on any `Foldable` of
  -- `Raster`s. You would likely want @foldl1'@ which is provided by both List
  -- and Vector. Keep in mind that `Raster` has a `Num` instance, so you can use
  -- all the normal math operators with them as well.
  , min, max
  -- *** Other
  -- | There is no binary form of these functions that exists without
  -- producing numerical error,  so you can't use the `foldl` family with these.
  -- Consider the average operation, where the following is /not/ true:
  -- \[
  --    \forall abc \in \mathbb{R}. \frac{\frac{a + b}{2} + c}{2} = \frac{a + b + c}{3}
  -- \]
  , mean, variety, majority, minority
  -- ** Focal Operations
  -- | Operations on one `Raster`, given some polygonal neighbourhood.
  ) where

import qualified Data.Array.Repa as R
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..), nub)
import qualified Data.Map.Strict as M
import           Data.Proxy (Proxy(..))
import           Data.Semigroup
import           GHC.TypeLits
import qualified Prelude as P
import           Prelude hiding (div, min, max, zipWith)

--

-- | A location on the Earth in some `Projection`.
data Point p a = Point { x :: a, y :: a } deriving (Eq, Show)

-- | The Earth is not a sphere. Various schemes have been invented
-- throughout history that provide `Point` coordinates for locations on the
-- earth, although all are approximations and come with trade-offs. We call
-- these "Projections", since they are a mapping of `Sphere` coordinates to
-- some other approximation. The Projection used most commonly for mapping on
-- the internet is called `WebMercator`.
--
-- A Projection is also known as a Coordinate Reference System (CRS).
--
-- Use `reproject` to convert `Point`s between various Projections.
class Projection p where
  -- | Convert a `Point` in this Projection to one of radians on a perfect `Sphere`.
  toSphere :: Point p Double -> Point Sphere Double

  -- | Convert a `Point` of radians on a perfect sphere to that of a specific Projection.
  fromSphere :: Point Sphere Double -> Point p Double

-- | Reproject a `Point` from one `Projection` to another.
reproject :: (Projection p, Projection r) => Point p Double -> Point r Double
reproject = fromSphere . toSphere

-- | A perfect geometric sphere. The earth isn't actually shaped this way,
-- but it's a convenient middle-ground for converting between various
-- `Projection`s.
data Sphere

instance Projection Sphere where
  toSphere = id
  fromSphere = id

data LatLng

instance Projection LatLng where
  toSphere = undefined
  fromSphere = undefined

-- | The most commonly used `Projection` for mapping in internet applications.
data WebMercator

instance Projection WebMercator where
  toSphere = undefined
  fromSphere = undefined

-- | A rectangular grid of data representing some area on the earth.
--
-- * @p@: What `Projection` is this Raster in?
-- * @c@: How many columns does this Raster have?
-- * @r@: How many rows does this Raster have?
-- * @a@: What data type is held in this Raster?
--
-- By having explicit p, c, and r, we make impossible any operation between
-- two Rasters of differing size or projection. Conceptually, we consider
-- Rasters of different size and projection to be /entirely different types/.
-- Example:
--
-- @
-- -- | A lazy 256x256 Raster with the value 5 at every index. Uses DataKinds
-- -- and "type literals" to achieve the same-size guarantee.
-- myRaster :: Raster WebMercator 256 256 Int
-- myRaster = constant 5
--
-- >>> length myRaster
-- 65536
-- @
newtype Raster p (c :: Nat) (r :: Nat) a = Raster { _array :: R.Array R.D R.DIM2 a }

instance Functor (Raster c r p) where
  fmap f (Raster a) = Raster $ R.map f a
  {-# INLINE fmap #-}

instance (Projection p, KnownNat c, KnownNat r) => Applicative (Raster p c r) where
  pure = constant
  {-# INLINE pure #-}

  -- TODO: Use strict ($)?
  fs <*> as = zipWith ($) fs as
  {-# INLINE (<*>) #-}

instance (Monoid a, Projection p, KnownNat c, KnownNat r) => Monoid (Raster p c r a) where
  mempty = constant mempty
  {-# INLINE mempty #-}

  a `mappend` b = zipWith mappend a b
  {-# INLINE mappend #-}

instance (Num a, Projection p, KnownNat c, KnownNat r) => Num (Raster p c r a) where
  a + b = zipWith (+) a b
  a - b = zipWith (-) a b
  a * b = zipWith (*) a b
  abs = fmap abs
  signum = fmap signum
  fromInteger = constant . fromInteger

instance (Fractional a, Projection p, KnownNat c, KnownNat r) => Fractional (Raster p c r a) where
  a / b = zipWith (/) a b
  fromRational = constant . fromRational

-- TODO: Use rules / checks to use `foldAllP` is the Raster has the right type / size.
-- TODO: Override `sum` and `product` with the builtins?
-- | Be careful - these operations will evaluate your lazy Raster. (Except
-- `length`, which has a specialized O(1) implementation.)
instance Foldable (Raster p c r) where
  foldMap f (Raster a) = R.foldAllS mappend mempty $ R.map f a
  sum (Raster a) = R.sumAllS a
  -- | O(1).
  length (Raster a) = R.size $ R.extent a

-- | O(1). Create a `Raster` of some size which has the same value everywhere.
constant :: forall c r a p. (KnownNat c, KnownNat r) => a -> Raster p c r a
constant a = Raster $ R.fromFunction sh (const a)
  where sh = R.ix2 (fromInteger $ natVal (Proxy :: Proxy c)) (fromInteger $ natVal (Proxy :: Proxy r))

-- | Called /LocalClassification/ in GaCM. The first argument is the value
-- to give to any index whose value is less than the lowest break in the `M.Map`.
--
-- This is a glorified `fmap` operation, but we expose it for convenience.
classify :: Ord a => b -> M.Map a b -> Raster p c r a -> Raster p c r b
classify def m r = fmap f r
  where f a = maybe def snd $ M.lookupLE a m

-- | Finds the minimum value at each index between two `Raster`s.
min :: Ord a => Raster p c r a -> Raster p c r a -> Raster p c r a
min (Raster a) (Raster b) = Raster $ R.zipWith P.min a b

-- | Finds the maximum value at each index between two `Raster`s.
max :: Ord a => Raster p c r a -> Raster p c r a -> Raster p c r a
max (Raster a) (Raster b) = Raster $ R.zipWith P.max a b

-- | Averages the values per-index of all `Raster`s in a collection.
mean :: (Projection p, Fractional a, KnownNat c, KnownNat r) => NonEmpty (Raster p c r a) -> Raster p c r a
mean (a :| as) = (/ len) <$> foldl' (+) a as
  where len = 1 + fromIntegral (length as)

-- | The count of unique values at each shared index.
variety :: (Projection p, KnownNat c, KnownNat r, Eq a) => NonEmpty (Raster p c r a) -> Raster p c r Int
variety = fmap (length . nub) . sequenceA

-- | The most frequently appearing value at each shared index.
majority :: (Projection p, KnownNat c, KnownNat r, Ord a) => NonEmpty (Raster p c r a) -> Raster p c r a
majority = fmap (fst . g . f) . sequenceA
  where f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty
        g = foldl1 (\(a,c) (k,v) -> if c < v then (k,v) else (a,c)) . M.toList

-- | The least frequently appearing value at each shared index.
minority :: (Projection p, KnownNat c, KnownNat r, Ord a) => NonEmpty (Raster p c r a) -> Raster p c r a
minority = fmap (fst . g . f) . sequenceA
  where f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty
        g = foldl1 (\(a,c) (k,v) -> if c > v then (k,v) else (a,c)) . M.toList

-- http://www.wikihow.com/Calculate-Variance
--variance :: [Raster p a] -> Raster
-- variance rs = undefined
--   where m = mean rs

-- Interesting... if `Raster` were a Monad (which it /should/ be), this
-- would just be `sequence`. The issue is that size isn't baked into the types,
-- only the types of the dimensions. Although any given `Array` does seem to
-- know what its own extent is.
-- (Addendum: should be `sequenceA`, so we actually only need Applicative)
--
-- Should size be baked into the types with DataKinds? If it were, you'd
-- think we'd be able to pull the size from the types to implement `pure` for
-- Applicative properly.
-- @newtype Raster c r p a = Raster { _array :: Raster DIM2 D a }@ ?
-- This would also guarantee that all the local ops would be well defined.
--listEm :: (Projection p, KnownNat c, KnownNat r) => NonEmpty (Raster p c r a) -> Raster p c r (NonEmpty a)
--listEm = sequenceA
--listEm (r :| rs) = foldl' (\acc s -> zipWith cons s acc) z rs
--  where z = (\a -> a :| []) <$> r
--{-# INLINE [2] listEm #-}

-- | Combine two `Raster`s, element-wise, with a binary operator.
zipWith :: Projection p => (a -> b -> d) -> Raster p c r a -> Raster p c r b -> Raster p c r d
zipWith f (Raster a) (Raster b) = Raster $ R.zipWith f a b
{-# INLINE zipWith #-}
