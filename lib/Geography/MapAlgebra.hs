{-# LANGUAGE DeriveAnyClass #-}
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
-- Here the main classes of operations ascribed to /Map Algebra/ and their
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
  -- | Operations between two or more `Raster`s. If the source Rasters aren't the
  -- same size, the size of the result will be their intersection. All operations
  -- are element-wise:
  --
  -- @
  -- add 1 1   2 2  ==  3 3
  --     1 1   2 2      3 3
  --
  -- mul 2 2   3 3  ==  6 6
  --     2 2   3 3      6 6
  -- @
  --
  -- If an operation you need isn't available here, use our `zipWith`:
  --
  -- @
  -- zipWith :: (a -> b -> c) -> Raster p a -> Raster p b -> Raster p c
  --
  -- -- Your operation, which you should INLINE and use bang patterns with.
  -- foo :: Int -> Int -> Int
  --
  -- bar :: Projection p => Raster p Int -> Raster p Int -> Raster p Int
  -- bar a b = zipWith foo a b
  -- @

  -- *** Unary
  -- | If you want to do simple unary @Raster -> Raster@ operations (called
  -- /LocalCalculation/ in GaCM), `Raster` is a `Functor` so you can use
  -- `fmap` as normal:
  --
  -- @
  -- myRaster :: Raster p Int
  -- abs :: Num a => a -> a
  --
  -- -- Absolute value of all values in the Raster.
  -- fmap abs myRaster
  -- @
  , classify
  -- *** Binary
  -- | You can safely use these with the `foldl` family on any `Foldable` of
  -- `Raster`s. You would likely want @foldl1'@ which is provided by both List
  -- and Vector.
  , add, sub, mul, div
  , min, max
  -- *** Other
  -- | There is no binary form of these functions that exists without
  -- producing numerical error,  so you can't use the `foldl` family with these.
  -- Consider the average operation, where the following is /not/ true:
  -- \[
  --    \forall abc \in \mathbf{R}. \frac{\frac{a + b}{2} + c}{2} = \frac{a + b + c}{3}
  -- \]
  , mean, variety, majority, minority
  -- ** Focal Operations
  -- | Operations on one `Raster`, given some polygonal neighbourhood.

  -- * Utilities
  , zipWith
  ) where

import qualified Data.Array.Repa as R
import           Data.Foldable
import           Data.List (nub, foldl1')
import qualified Data.Map.Strict as M
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

newtype Raster p a = Raster { _array :: R.Array R.D R.DIM2 a }

instance Functor (Raster p) where
  fmap f (Raster a) = Raster $ R.map f a
  {-# INLINE fmap #-}

-- TODO: Use rules / checks to use `foldAllP` is the Raster has the right type / size.
-- TODO: Override `sum` and `product` with the builtins?
-- | Be careful - these operations will evaluate your lazy Raster.
instance Foldable (Raster p) where
  foldMap f (Raster a) = R.foldAllS mappend mempty $ R.map f a

-- TODO: How?
-- instance Traversable (Raster p) where
--   traverse f (Raster a) = undefined

-- | O(1). Create a `Raster` of some size which has the same value everywhere.
constant :: R.DIM2 -> a -> Raster p a
constant sh n = Raster $ R.fromFunction sh (const n)

-- TODO: Use a BSTree instead (wherever those come from).
-- | Called /LocalClassification/ in GaCM.
classify :: Ord a => M.Map a b -> Raster p a -> Raster p b
classify = undefined

add :: (Projection p, Num a) => Raster p a -> Raster p a -> Raster p a
add (Raster a) (Raster b) = Raster $ R.zipWith (+) a b

sub :: (Projection p, Num a) => Raster p a -> Raster p a -> Raster p a
sub (Raster a) (Raster b) = Raster $ R.zipWith (-) a b

mul :: (Projection p, Num a) => Raster p a -> Raster p a -> Raster p a
mul (Raster a) (Raster b) = Raster $ R.zipWith (*) a b

div :: (Projection p, Fractional a) => Raster p a -> Raster p a -> Raster p a
div (Raster a) (Raster b) = Raster $ R.zipWith (/) a b

min :: (Projection p, Ord a) => Raster p a -> Raster p a -> Raster p a
min (Raster a) (Raster b) = Raster $ R.zipWith P.min a b

max :: (Projection p, Ord a) => Raster p a -> Raster p a -> Raster p a
max (Raster a) (Raster b) = Raster $ R.zipWith P.max a b

{- OPS TO ADD

variance

-}

-- | Averages the values per-index of all `Raster`s in a collection.
mean :: (Projection p, Fractional b) => [Raster p b] -> Raster p b
mean rs = (/ len) <$> foldl1' add rs
  where len = fromIntegral $ length rs

-- | The count of unique values at each shared index.
variety :: Eq a => [Raster p a] -> Raster p Int
variety = fmap (length . nub) . listEm

-- | The most frequently appearing value at each shared index.
majority :: Ord a => [Raster p a] -> Raster p a
majority = fmap (fst . g . f) . listEm
  where f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty
        g = foldl1' (\(a,c) (k,v) -> if c < v then (k,v) else (a,c)) . M.toList

-- | The least frequently appearing value at each shared index.
minority :: Ord a => [Raster p a] -> Raster p a
minority =  fmap (fst . g . f) . listEm
  where f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty
        g = foldl1' (\(a,c) (k,v) -> if c > v then (k,v) else (a,c)) . M.toList

-- Interesting... if `Raster` were a Monad (which it /should/ be), this
-- would just be `sequence`. The issue is that size isn't baked into the types,
-- only the types of the dimensions. Although any given `Array` does seem to
-- know what its own extent is.
--
-- Should size be baked into the types with DataKinds? If it were, you'd
-- think we'd be able to pull the size from the types to implement `pure` for
-- Applicative properly.
-- @newtype Raster c r p a = Raster { _array :: Raster DIM2 D a }@ ?
-- This would also guarantee that all the local ops would be well defined.
listEm :: [Raster p a] -> Raster p [a]
listEm (r:rs) = foldl' (\acc s -> zipWith (:) s acc) z rs
  where z = (: []) <$> r
{-# INLINE [2] listEm #-}

-- | Combine two `Raster`s, element-wise, with a binary operator. If the
-- extent of the two array arguments differ, then the resulting Raster's extent
-- is their intersection.
zipWith :: (a -> b -> c) -> Raster p a -> Raster p b -> Raster p c
zipWith f (Raster a) (Raster b) = Raster $ R.zipWith f a b
