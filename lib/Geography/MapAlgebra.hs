{-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE Strict #-}  -- Does this improve performance?

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
  -- | If you want to do simple unary @Raster -> Raster@ operations,
  -- `Raster` is a `Functor` so you can use `fmap` as normal:
  --
  -- @
  -- myRaster :: Raster p Int
  --
  -- -- Absolute value of all values in the Raster.
  -- fmap abs myRaster
  -- @

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
  -- If an operation you need isn't available here, the trick is to use our
  -- `zipWith`:
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

  -- *** Foldable Operations
  -- | You can safely use these with the `foldl` family on any `Foldable` of
  -- `Raster`s. You would likely want @foldl1'@ which is provided by both List
  -- and Vector.
  , add, sub, mul, div
  , min, max
  -- *** Unfoldable Operations
  -- | There is no binary form of these functions that exist without
  -- producing numerical error,  so you can't use the `foldl` family with these.
  -- Consider the average operation: @(((a + b) \/ 2) + c) \/ 2@ is not equal to
  -- @(a + b + c) \/ 3@.
  , mean
  -- ** Focal Operations
  -- | Operations on one `Raster`, given some polygonal neighbourhood.

  -- * Utilities
  , zipWith
  ) where

import qualified Data.Array.Repa as R
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
class Projection p where
  toSphere :: Point p Double -> Point Sphere Double
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

mask

-- We probably need additional zipWith3, etc, for these.
mean, mean3, mean4 ?
variance
variety (count of unique values at each location)

-}

-- TODO: Use rewrite rules to specialize on strict foldl1 provided by List and Vector!
-- | Averages the values per-index of all `Raster`s in a collection.
mean :: (Projection p, Foldable t, Fractional b) => t (Raster p b) -> Raster p b
mean rs = (/ len) <$> foldl1 add rs
  where len = fromIntegral $ length rs

-- | Combine two `Raster`s, element-wise, with a binary operator. If the
-- extent of the two array arguments differ, then the resulting Raster's extent
-- is their intersection.
zipWith :: (a -> b -> c) -> Raster p a -> Raster p b -> Raster p c
zipWith f (Raster a) (Raster b) = Raster $ R.zipWith f a b
