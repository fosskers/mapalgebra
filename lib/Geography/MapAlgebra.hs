{-# LANGUAGE Rank2Types, DataKinds, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, TupleSections, ApplicativeDo #-}

-- |
-- Module    : Geography.MapAlgebra
-- Copyright : (c) Colin Woodbury, 2018
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
-- * /Focal Operations/ -> More general than /convolution/; 'massiv' has support for this (described below)
-- * /Zonal Operations/ -> ??? TODO
--
-- Whether it is meaningful to perform operations between two given
-- `Raster`s (i.e. whether the Rasters properly overlap on the earth) is not
-- handled in this library and is left to the application.
--
-- The "colour ramp" generation functions (like `greenRed`) gratefully borrow colour
-- sets from Gretchen N. Peterson's book /Cartographer's Toolkit/.

{- TODO

Benchmark against numpy as well as GT's collections API

-}

module Geography.MapAlgebra
  (
  -- * Types
  -- ** Rasters
    Raster(..)
  , lazy, strict
  , RGBARaster(..)
  -- *** Creation
  , constant, fromFunction, fromVector, fromRGBA, fromGray
  -- *** Colouring
  -- | The `M.Map`s here can be used with `classify` to
  --   transform /ranges/ of values into certain colours in \(\mathcal{O}(n\log(n))\).
  --   Each Map-generating function (like `greenRed`) creates a "colour ramp" of 10 colours. So, it expects
  --   to be given a list of 10 "break points" which become the Map's keys. Any less than 10 will result
  --   in the later colours not being used. Any more than 10 will be ignored. The list of break points is
  --   assumed to be sorted.
  --   `invisible` can be used as the default value to `classify`, to make invisible any value that falls outside
  --   the range of the Maps.
  --
  -- If you aren't interested in colour but still want to render your `Raster`,
  -- consider `grayscale`. Coloured `Raster`s can be unwrapped with `_array` and then
  -- output with functions like `writeImage`.
  , invisible
  , greenRed, spectrum, blueGreen, purpleYellow, brownBlue
  , grayBrown, greenPurple, brownYellow, purpleGreen, purpleRed
  -- *** Output and Display
  -- | For coloured output, first use `classify` over your `Raster` to produce a
  -- @Raster u p r c (Pixel RGBA Word8)@. Then unwrap it with `_array` and output
  -- with something like `writeImage`.
  --
  -- For quick debugging, you can visualize a `Raster` with `display`.
  , grayscale
  , writeImage, writeImageAuto
  , display
  -- ** Projections
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
  -- zipWith :: (a -> b -> d) -> Raster p r c a -> Raster p r c b -> Raster p r c d
  --
  -- -- Your operation, which you should INLINE and use bang patterns with.
  -- foo :: Int -> Int -> Int
  --
  -- bar :: Projection p => Raster p r c Int -> Raster p r c Int -> Raster p r c Int
  -- bar a b = zipWith foo a b
  -- @
  , zipWith
  -- *** Unary
  -- | If you want to do simple unary @Raster -> Raster@ operations (called
  -- /LocalCalculation/ in GaCM), `Raster` is a `Functor` so you can use
  -- `fmap` as normal:
  --
  -- @
  -- myRaster :: Raster p r c Int
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
  , lmin, lmax
  -- *** Other
  -- | There is no binary form of these functions that exists without
  -- producing numerical error,  so you can't use the `foldl` family with these.
  -- Consider the average operation, where the following is /not/ true:
  -- \[
  --    \forall abc \in \mathbb{R}. \frac{\frac{a + b}{2} + c}{2} = \frac{a + b + c}{3}
  -- \]
  , lmean, lvariety, lmajority, lminority, lvariance
  -- ** Focal Operations
  -- | Operations on one `Raster`, given some polygonal neighbourhood.
  -- Your `Raster` must be of a `Manifest` type (i.e. backed by real memory) before
  -- you attempt any focal operations. Without this constraint, wayward users
  -- run the risk of setting up operations that would perform terribly.
  -- Use `strict` to easily convert a lazy `Raster` to a memory-backed one.
  --
  -- @
  -- myRaster :: Raster D p r c Float
  --
  -- averaged :: Raster DW p r c Float
  -- averaged = fmean $ strict P myRaster
  -- @
  , fclassify
  , fsum, fmean
  , fmax, fmin
  , fmajority, fminority, fvariety
  , fpercentage, fpercentile
  -- *** Lineal
  -- | Focal operations that assume that groups of data points represent line-like objects
  -- in a `Raster`. GaCM calls these /lineal characteristics/ and describes them fully
  -- on page 18 and 19.
  , Direction(..)
  , flinkage, flength
  -- *** Areal
  -- | Focal operations that assume that groups of data points represent 2D areas
  -- in a `Raster`. GaCM calls these /areal characteristics/ and describes them fully
  -- on page 20 and 21.
  , Cell(..), Corners(..), Surround(..)
  , fpartition, fshape, ffrontage, farea
  -- *** Surficial
  -- | Focal operations that work over elevation `Raster`s. GaCM calls elevation
  -- features /surficial characteristics/ and describes them fully on page 21
  -- and 22.
  --
  -- Some of these operations require finding a "best-fit plane" that
  -- approximates the surficial shape of each pixel. Each pixel has 9 "facet points"
  -- calculated for it based on its surrounding pixels. We then use these facets to determine
  -- a plane which adheres to this equation:
  --
  -- \[
  -- ax + by + c = z
  -- \]
  -- This is a linear equation that we can solve for in the form \(Ax = B\).
  -- For facet points \((x_i, y_i, z_i)\), we have:
  --
  -- \[
  -- \begin{bmatrix}
  -- x_0 & y_0 & 1 \\
  -- x_1 & y_1 & 1 \\
  -- \vdots & \vdots & \vdots \\
  -- x_n & y_n & 1
  -- \end{bmatrix} \begin{bmatrix}
  -- a\\
  -- b\\
  -- c
  -- \end{bmatrix} = \begin{bmatrix}
  -- z_0\\
  -- z_1\\
  -- \vdots\\
  -- z_n
  -- \end{bmatrix}
  -- \]
  --
  -- Since this system of equations is "over determined", we rework the above to
  -- find the coefficients of the best-fitting plane via:
  -- \[
  --    \begin{bmatrix}
  --        a\\
  --        b\\
  --        c
  --    \end{bmatrix} = \boxed{(A^{T}A)^{-1}A^{T}}B
  -- \]
  -- The boxed section is called the "left pseudo inverse" and is available as `leftPseudo`.
  -- The actual values of \(A\) don't matter for our purposes, hence \(A\) can be fixed to
  -- avoid redundant calculations.
  , fvolume, fgradient, faspect
  -- * Utilities
  , leftPseudo, tau
  ) where

import           Control.Concurrent (getNumCapabilities)
import           Control.DeepSeq (NFData)
import           Data.Bool (bool)
import           Data.Default (Default, def)
import           Data.Foldable
import           Data.Int
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Massiv.Array as A
import           Data.Massiv.Array hiding (zipWith)
import           Data.Massiv.Array.IO
import qualified Data.Massiv.Array.Manifest.Vector as A
import           Data.Maybe (mapMaybe)
import           Data.Proxy (Proxy(..))
import           Data.Semigroup
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import qualified Data.Vector.Generic as GV
import           Data.Word
import           GHC.TypeLits
import           Graphics.ColorSpace (Elevator, RGBA, Y, Pixel(..))
import qualified Numeric.LinearAlgebra as LA
import qualified Prelude as P
import           Prelude hiding (zipWith)
import           Text.Printf (printf)

--

-- | A location on the Earth in some `Projection`.
data Point p = Point { x :: !Double, y :: !Double } deriving (Eq, Show)

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
  toSphere :: Point p -> Point Sphere

  -- | Convert a `Point` of radians on a perfect sphere to that of a specific Projection.
  fromSphere :: Point Sphere -> Point p

-- | Reproject a `Point` from one `Projection` to another.
reproject :: (Projection p, Projection r) => Point p -> Point r
reproject = fromSphere . toSphere
{-# INLINE reproject #-}

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
-- * @u@: What is the /underlying representation/ of this Raster? (see 'massiv')
-- * @p@: What `Projection` is this Raster in?
-- * @r@: How many rows does this Raster have?
-- * @c@: How many columns does this Raster have?
-- * @a@: What data type is held in this Raster?
--
-- By having explicit p, r, and c, we make impossible any operation between
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
newtype Raster u p (r :: Nat) (c :: Nat) a = Raster { _array :: Array u Ix2 a }

-- | Warning: This will evaluate (at most) the 10x10 top-left corner of your
-- `Raster` for display. This should only be used for debugging.
instance (Show a, Load (EltRepr u Ix2) Ix2 a, Size u Ix2 a) => Show (Raster u p r c a) where
  show (Raster a) = show . computeAs B $ extract' zeroIndex (r :. c) a
    where (r :. c) = liftIndex (P.min 10) $ size a

instance (Eq a, Unbox a) => Eq (Raster U p r c a) where
  Raster a == Raster b = a == b
  {-# INLINE (==) #-}

instance (Eq a, Storable a) => Eq (Raster S p r c a) where
  Raster a == Raster b = a == b
  {-# INLINE (==) #-}

instance (Eq a, Prim a) => Eq (Raster P p r c a) where
  Raster a == Raster b = a == b
  {-# INLINE (==) #-}

instance (Eq a, NFData a) => Eq (Raster N p r c a) where
  Raster a == Raster b = a == b
  {-# INLINE (==) #-}

instance Eq a => Eq (Raster B p r c a) where
  Raster a == Raster b = a == b
  {-# INLINE (==) #-}

instance Eq a => Eq (Raster D p r c a) where
  Raster a == Raster b = a == b
  {-# INLINE (==) #-}

instance Functor (Raster DW p r c) where
  fmap f (Raster a) = Raster $ fmap f a
  {-# INLINE fmap #-}

instance Functor (Raster D p r c) where
  fmap f (Raster a) = Raster $ fmap f a
  {-# INLINE fmap #-}

instance Functor (Raster DI p r c) where
  fmap f (Raster a) = Raster $ fmap f a
  {-# INLINE fmap #-}

instance (KnownNat r, KnownNat c) => Applicative (Raster D p r c) where
  pure = constant D Par
  {-# INLINE pure #-}

  -- TODO: Use strict ($)?
  fs <*> as = zipWith ($) fs as
  {-# INLINE (<*>) #-}

instance Semigroup a => Semigroup (Raster D p r c a) where
  a <> b = zipWith (<>) a b
  {-# INLINE (<>) #-}

instance (Monoid a, KnownNat r, KnownNat c) => Monoid (Raster D p r c a) where
  mempty = constant D Par mempty
  {-# INLINE mempty #-}

  a `mappend` b = zipWith mappend a b
  {-# INLINE mappend #-}

instance (Num a, KnownNat r, KnownNat c) => Num (Raster D p r c a) where
  a + b = zipWith (+) a b
  {-# INLINE (+) #-}

  a - b = zipWith (-) a b
  {-# INLINE (-) #-}

  a * b = zipWith (*) a b
  {-# INLINE (*) #-}

  abs = fmap abs
  signum = fmap signum
  fromInteger = constant D Par . fromInteger

instance (Fractional a, KnownNat r, KnownNat c) => Fractional (Raster D p r c a) where
  a / b = zipWith (/) a b
  {-# INLINE (/) #-}

  fromRational = constant D Par . fromRational

-- TODO: more explicit implementations?
-- | `length` has a specialized \(\mathcal{O}(1)\) implementation.
instance Foldable (Raster D p r c) where
  foldMap f (Raster a) = foldMap f a
  {-# INLINE foldMap #-}

  sum (Raster a) = A.sum a
  {-# INLINE sum #-}

  product (Raster a) = A.product a
  {-# INLINE product #-}

  -- | \(\mathcal{O}(1)\).
  length (Raster a) = (\(r :. c) -> r * c) $ A.size a
  {-# INLINE length #-}

-- | \(\mathcal{O}(1)\). Force a `Raster`'s representation to `D`, allowing it
-- to undergo various operations. All operations between `D` `Raster`s are fused
-- and allocate no extra memory.
lazy :: Source u Ix2 a => Raster u p r c a -> Raster D p r c a
lazy (Raster a) = Raster $ delay a
{-# INLINE lazy #-}

-- | Evaluate some lazy (`D`, `DW`, or `DI`) `Raster` to some explicit `Manifest` type
-- (i.e. to a real memory-backed Array). Will follow the `Comp`utation strategy
-- of the underlying 'massiv' `Array`.
--
-- __Note:__ If using the `Par` computation strategy, make sure you're compiling with
-- @-with-rtsopts=-N@ to automatically use all available CPU cores at runtime. Otherwise
-- your "parallel" operations will only execute on one core.
strict :: (Load v Ix2 a, Mutable u Ix2 a) => u -> Raster v p r c a -> Raster u p r c a
strict u (Raster a) = Raster $ computeAs u a
{-# INLINE strict #-}

-- | Create a `Raster` of any size which has the same value everywhere.
constant :: (KnownNat r, KnownNat c, Construct u Ix2 a) => u -> Comp -> a -> Raster u p r c a
constant u c a = fromFunction u c (const a)
{-# INLINE constant #-}

-- | \(\mathcal{O}(1)\). Create a `Raster` from a function of its row and column number respectively.
fromFunction :: forall u p r c a. (KnownNat r, KnownNat c, Construct u Ix2 a) =>
  u -> Comp -> (Ix2 -> a) -> Raster u p r c a
fromFunction u c f = Raster $ makeArrayR u c sh f
  where sh = fromInteger (natVal (Proxy :: Proxy r)) :. fromInteger (natVal (Proxy :: Proxy c))
{-# INLINE fromFunction #-}

-- | \(\mathcal{O}(1)\). Create a `Raster` from the values of any `GV.Vector` type.
-- Will fail if the size of the Vector does not match the declared size of the `Raster`.
fromVector :: forall v p r c a. (KnownNat r, KnownNat c, GV.Vector v a, Mutable (A.ARepr v) Ix2 a, Typeable v) =>
  Comp -> v a -> Either String (Raster (A.ARepr v) p r c a)
fromVector comp v | (r * c) == GV.length v = Right . Raster $ A.fromVector comp (r :. c) v
                  | otherwise = Left $ printf "Expected Pixel Count: %d - Actual: %d" (r * c) (GV.length v)
  where r = fromInteger $ natVal (Proxy :: Proxy r)
        c = fromInteger $ natVal (Proxy :: Proxy c)

-- | An RGBA image whose colour bands are distinct. Since each band starts as `D`,
-- any band you don't use won't consume extra memory.
data RGBARaster p r c a = RGBARaster { _red   :: Raster D p r c a
                                     , _green :: Raster D p r c a
                                     , _blue  :: Raster D p r c a
                                     , _alpha :: Raster D p r c a }

-- | Read any image type into a `Raster` of distinct colour bands
-- with the cell type you declare. If the source image stores its
-- values as `Int` but you declare `Double`, the conversion will happen
-- automatically.
--
-- Will fail if the declared size of the `Raster`
-- does not match the actual size of the input image.
fromRGBA :: forall p r c a. (Elevator a, KnownNat r, KnownNat c) => FilePath -> IO (Either String (RGBARaster p r c a))
fromRGBA fp = do
  cap <- getNumCapabilities
  img <- setComp (bool Par Seq $ cap == 1) <$> readImageAuto fp
  let rows = fromInteger $ natVal (Proxy :: Proxy r)
      cols = fromInteger $ natVal (Proxy :: Proxy c)
      (r :. c) = size img
  pure . bool (Left $ printf "Expected Size: %d x %d - Actual Size: %d x %d" rows cols r c) (Right $ f img) $ r == rows && c == cols
  where f :: Image S RGBA a -> RGBARaster p r c a
        f (delay -> img) = RGBARaster (Raster $ fmap (\(PixelRGBA r _ _ _) -> r) img)
                                      (Raster $ fmap (\(PixelRGBA _ g _ _) -> g) img)
                                      (Raster $ fmap (\(PixelRGBA _ _ b _) -> b) img)
                                      (Raster $ fmap (\(PixelRGBA _ _ _ a) -> a) img)

-- TODO: How to unify these two functions?

-- | Read a grayscale image. If the source file has more than one colour band,
-- they'll be combined automatically.
fromGray :: forall p r c a. (Elevator a, KnownNat r, KnownNat c) => FilePath -> IO (Either String (Raster D p r c a))
fromGray fp = do
  cap <- getNumCapabilities
  img <- setComp (bool Par Seq $ cap == 1) <$> readImageAuto fp
  let rows = fromInteger $ natVal (Proxy :: Proxy r)
      cols = fromInteger $ natVal (Proxy :: Proxy c)
      (r :. c) = size img
  pure . bool (Left $ printf "Expected Size: %d x %d - Actual Size: %d x %d" rows cols r c) (Right $ f img) $ r == rows && c == cols
  where f :: Image S Y a -> Raster D p r c a
        f (delay -> img) = Raster $ (\(PixelY a) -> a) <$> img

-- | An invisible pixel (alpha channel set to 0).
invisible :: Pixel RGBA Word8
invisible = PixelRGBA 0 0 0 0

-- | Construct a colour ramp.
-- ramp :: Ord k => [(Word8, Word8, Word8)] -> [k] -> M.Map k PixelRGBA8
ramp :: Ord k => [(Word8, Word8, Word8)] -> [k] -> M.Map k (Pixel RGBA Word8)
ramp colours breaks = M.fromList . P.zip breaks $ P.map (\(r,g,b) -> PixelRGBA r g b maxBound) colours
{-# INLINE ramp #-}

-- | From page 32 of /Cartographer's Toolkit/.
greenRed :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
greenRed = ramp colours
  where colours = [ (0, 48, 0), (31, 79, 20), (100, 135, 68), (148, 193, 28), (193, 242, 3)
                  , (241, 255, 159), (249, 228, 227), (202, 145, 150), (153, 101, 97), (142, 38 ,18) ]

-- | From page 33 of /Cartographer's Toolkit/.
spectrum :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
spectrum = ramp colours
  where colours = [ (0, 22, 51), (51, 18, 135), (150, 0, 204), (242, 13, 177), (255, 61, 61)
                  , (240, 152, 56), (248, 230, 99), (166, 249, 159), (184, 249, 212), (216, 230, 253) ]

-- | From page 34 of /Cartographer's Toolkit/.
blueGreen :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
blueGreen = ramp colours
  where colours = [ (29, 43, 53), (37, 44, 95), (63, 70, 134), (89, 112, 147), (87, 124, 143)
                  , (117, 160, 125), (188, 219, 173), (239, 253, 163), (222, 214, 67), (189, 138, 55) ]

-- | From page 35 of /Cartographer's Toolkit/.
purpleYellow :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
purpleYellow = ramp colours
  where colours = [ (90, 89, 78), (73, 65, 132), (107, 86, 225), (225, 67, 94), (247, 55, 55)
                  , (251, 105, 46), (248, 174, 66), (249, 219, 25), (255, 255, 0), (242, 242, 242) ]

-- | From page 36 of /Cartographer's Toolkit/.
brownBlue :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
brownBlue = ramp colours
  where colours = [ (27, 36, 43), (86, 52, 42), (152, 107, 65), (182, 176, 152), (215, 206, 191)
                  , (198, 247, 0), (53, 227, 0), (30, 158, 184), (22, 109, 138), (12, 47, 122) ]

-- | From page 37 of /Cartographer's Toolkit/.
grayBrown :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
grayBrown = ramp colours
  where colours = [ (64, 57, 88), (95, 96, 116), (158, 158, 166), (206, 208, 197), (215, 206, 191)
                  , (186, 164, 150), (160, 124, 98), (117, 85, 72), (90, 70, 63), (39, 21, 17) ]

-- | From page 38 of /Cartographer's Toolkit/.
greenPurple :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
greenPurple = ramp colours
  where colours = [ (89, 168, 15), (158, 213, 76), (196, 237, 104), (226, 255, 158), (240, 242, 221)
                  , (248, 202, 140), (233, 161, 137), (212, 115, 132), (172, 67, 123), (140, 40, 110) ]

-- | From page 39 of /Cartographer's Toolkit/.
brownYellow :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
brownYellow = ramp colours
  where colours = [ (96, 72, 96), (120, 72, 96), (168, 96, 96), (192, 120, 96), (240, 168, 72)
                  , (248, 202, 140), (254, 236, 174), (255, 244, 194), (255, 247, 219), (255, 252, 246) ]

-- | From page 40 of /Cartographer's Toolkit/.
purpleGreen :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
purpleGreen = ramp colours
  where colours = [ (80, 73, 113), (117, 64, 152), (148, 116, 180), (199, 178, 214), (223, 204, 228)
                  , (218, 234, 193), (171, 214, 155), (109, 192, 103), (13, 177, 75), (57, 99, 83) ]

-- | From page 41 of /Cartographer's Toolkit/.
purpleRed :: Ord k => [k] -> M.Map k (Pixel RGBA Word8)
purpleRed = ramp colours
  where colours = [ (51, 60, 255), (76, 60, 233), (99, 60, 211), (121, 60, 188), (155, 60, 155)
                  , (166, 60, 143), (188, 60, 121), (206, 60, 94), (217, 60, 83), (255, 60, 76) ]

-- | Convert a `Raster` into a grayscale pixels, suitable for easy output with functions
-- like `writeImage`.
grayscale :: Functor (Raster u p r c) => Raster u p r c a -> Raster u p r c (Pixel Y a)
grayscale = fmap PixelY
{-# INLINE grayscale #-}

-- | View a `Raster` as grayscale with the default image viewer of your OS.
--
-- For more direct control, consider `displayImage` from 'massiv-io'.
display :: (Functor (Raster u p r c), Load u Ix2 (Pixel Y a), Elevator a) => Raster u p r c a -> IO ()
display = displayImage . computeAs S . _array . grayscale

-- | Called /LocalClassification/ in GaCM. The first argument is the value
-- to give to any index whose value is less than the lowest break in the `M.Map`.
--
-- This is a glorified `fmap` operation, but we expose it for convenience.
classify :: (Ord a, Functor f) => b -> M.Map a b -> f a -> f b
classify d m r = fmap f r
  where f a = maybe d snd $ M.lookupLE a m

-- | Finds the minimum value at each index between two `Raster`s.
lmin :: (Ord a, Source u Ix2 a) => Raster u p r c a -> Raster u p r c a -> Raster D p r c a
lmin = zipWith P.min

-- | Finds the maximum value at each index between two `Raster`s.
lmax :: (Ord a, Source u Ix2 a) => Raster u p r c a -> Raster u p r c a -> Raster D p r c a
lmax = zipWith P.max

-- | Averages the values per-index of all `Raster`s in a collection.
lmean :: (Fractional a, KnownNat r, KnownNat c) => NonEmpty (Raster D p r c a) -> Raster D p r c a
lmean (a :| as) = (/ len) <$> foldl' (+) a as
  where len = 1 + fromIntegral (length as)

-- | The count of unique values at each shared index.
lvariety :: (KnownNat r, KnownNat c, Eq a) => NonEmpty (Raster D p r c a) -> Raster D p r c Int
lvariety = fmap (length . NE.nub) . sequenceA

-- | The most frequently appearing value at each shared index.
lmajority :: (KnownNat r, KnownNat c, Ord a) => NonEmpty (Raster D p r c a) -> Raster D p r c a
lmajority = fmap majo . sequenceA

-- | Find the most common value in some `Foldable`.
majo :: (Foldable t, Ord a) => t a -> a
majo = fst . g . f
  where f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty
        g = foldl1 (\(a,c) (k,v) -> if c < v then (k,v) else (a,c)) . M.toList
{-# INLINE majo #-}

-- | The least frequently appearing value at each shared index.
lminority :: (KnownNat r, KnownNat c, Ord a) => NonEmpty (Raster D p r c a) -> Raster D p r c a
lminority = fmap mino . sequenceA

-- | Find the least common value in some `Foldable`.
mino :: (Foldable t, Ord a) => t a -> a
mino = fst . g . f
  where f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty
        g = foldl1 (\(a,c) (k,v) -> if c > v then (k,v) else (a,c)) . M.toList
{-# INLINE mino #-}

-- | A measure of how spread out a dataset is. This calculation will fail
-- with `Nothing` if a length 1 list is given.
lvariance :: (KnownNat r, KnownNat c, Fractional a) => NonEmpty (Raster D p r c a) -> Maybe (Raster D p r c a)
lvariance (_ :| []) = Nothing
lvariance rs = Just (f <$> sequenceA rs)
  where len = fromIntegral $ length rs
        avg ns = (/ len) $ P.sum ns  -- TODO: Use foldl'
        f os@(n :| ns) = foldl' (\acc m -> acc + ((m - av) ^ 2)) ((n - av) ^ 2) ns / (len - 1)
          where av = avg os

-- Old implementation that was replaced with `sequenceA` usage above. I wonder if this is faster?
-- Leaving it here in case we feel like comparing later.
--listEm :: (Projection p, KnownNat r, KnownNat c) => NonEmpty (Raster p r c a) -> Raster p r c (NonEmpty a)
--listEm = sequenceA
--listEm (r :| rs) = foldl' (\acc s -> zipWith cons s acc) z rs
--  where z = (\a -> a :| []) <$> r
--{-# INLINE [2] listEm #-}

-- | Combine two `Raster`s, element-wise, with a binary operator.
zipWith :: (Source u Ix2 a, Source u Ix2 b) =>
  (a -> b -> d) -> Raster u p r c a -> Raster u p r c b -> Raster D p r c d
zipWith f (Raster a) (Raster b) = Raster $ A.zipWith f a b
{-# INLINE zipWith #-}

sumStencil :: (Num a, Default a) => Stencil Ix2 a a
sumStencil = makeStencil (Fill 0) (3 :. 3) (1 :. 1) $ \f ->
  f (-1 :. -1) + f (-1 :. 0) + f (-1 :. 1) +
  f (0  :. -1) + f (0  :. 0) + f (0  :. 1) +
  f (1  :. -1) + f (1  :. 0) + f (1  :. 1)
{-# INLINE sumStencil #-}

-- | Focal Classification - full control over every value in the neighbourhood.
fclassify :: (Default a, Manifest u Ix2 a) => ([a] -> b) -> Border a -> Raster u p r c a -> Raster DW p r c b
fclassify f e (Raster a) = Raster $ mapStencil (groupStencil f e) a
{-# INLINE fclassify #-}

-- | Focal Addition.
fsum :: (Num a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fsum (Raster a) = Raster $ mapStencil sumStencil a
{-# INLINE fsum #-}

-- | Focal Mean.
fmean :: (Fractional a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmean = fmap (/ 9) . fsum
{-# INLINE fmean #-}

-- TODO: Use `NonEmpty`?
groupStencil :: Default a => ([a] -> b) -> Border a -> Stencil Ix2 a b
groupStencil f e = makeStencil e (3 :. 3) (1 :. 1) $ \g -> f <$> P.traverse g ixs
  where ixs = (:.) <$> [-1 .. 1] <*> [-1 .. 1]
{-# INLINE groupStencil #-}

-- | Focal Maximum.
fmax :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmax = fclassify P.maximum Edge
{-# INLINE fmax #-}

-- | Focal Minimum.
fmin :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmin = fclassify P.minimum Edge
{-# INLINE fmin #-}

-- | Focal Variety - the number of unique values in each neighbourhood.
fvariety :: (Eq a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Int
fvariety = fclassify (length . L.nub) Edge
{-# INLINE fvariety #-}

-- | Focal Majority - the most frequently appearing value in each neighbourhood.
fmajority :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmajority = fclassify majo Continue
{-# INLINE fmajority #-}

-- | Focal Minority - the least frequently appearing value in each neighbourhood.
fminority :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fminority = fclassify mino Continue
{-# INLINE fminority #-}

-- | TODO: Rename this.
percStencil :: Default a => (a -> [a] -> b) -> Border a -> Stencil Ix2 a b
percStencil f e = makeStencil e (3 :. 3) (1 :. 1) $ \g ->
  f <$> g (0 :. 0) <*> sequenceA [ g (-1 :. -1), g (-1 :. 0), g (-1 :. 1)
                                 , g (0  :. -1),              g (0  :. 1)
                                 , g (1  :. -1), g (1  :. 0), g (1  :. 1) ]
{-# INLINE percStencil #-}

-- | Focal Percentage - the percentage of neighbourhood values that are equal
-- to the neighbourhood focus. Not to be confused with `fpercentile`.
fpercentage :: (Eq a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Double
fpercentage (Raster a) = Raster $ mapStencil (percStencil f Continue) a
  where f focus vs = fromIntegral (length $ filter (== focus) vs) / 8

-- | Focal Percentile - the percentage of neighbourhood values that are /less/
-- than the neighbourhood focus. Not to be confused with `fpercentage`.
fpercentile :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Double
fpercentile (Raster a) = Raster $ mapStencil (percStencil f Continue) a
  where f focus vs = fromIntegral (length $ filter (< focus) vs) / 8

-- | Focal Linkage - a description of how each neighbourhood focus is connected
-- to its neighbours. Foci of equal value to none of their neighbours will have
-- an empty `S.Set`.
flinkage :: (Default a, Eq a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c (S.Set Direction)
flinkage (Raster a) = Raster $ mapStencil linkStencil a

-- `Fill def` has the highest chance of the edge pixel and the off-the-edge pixel
-- having a different value. This is until the following is addressed:
-- https://github.com/fosskers/mapalgebra/pull/3#issuecomment-379943208
linkStencil :: (Default a, Eq a) => Stencil Ix2 a (S.Set Direction)
linkStencil = makeStencil (Fill def) (3 :. 3) (1 :. 1) $ \f ->
  let focus = f (0 :. 0)
      axes  = S.fromList $ mapMaybe (\(Pair ix d) -> bool Nothing (Just d) $ focus == f ix) ti
      diag (Pair ix d) | L.any (`S.member` axes) (ignores d) = Nothing
                       | focus == f ix = Just d
                       | otherwise = Nothing
      diags | S.size axes > 2 = mempty
            | otherwise = S.fromList $ mapMaybe diag ex
  in pure $ axes <> diags
  where ti = [ Pair (0  :. 1)  East
             , Pair (-1 :. 0)  North
             , Pair (0  :. -1) West
             , Pair (1  :. 0)  South ]
        ex = [ Pair (-1 :. 1)  NorthEast
             , Pair (-1 :. -1) NorthWest
             , Pair (1  :. -1) SouthWest
             , Pair (1  :. 1)  SouthEast ]
{-# INLINE linkStencil #-}

ignores :: Direction -> [Direction]
ignores NorthWest = [ North, West ]
ignores SouthWest = [ West, South ]
ignores SouthEast = [ South, East ]
ignores NorthEast = [ North, East ]
ignores _         = []

-- | Directions that neighbourhood foci can be connected by. See `flinkage`
-- and `flength`.
data Direction = East | NorthEast | North | NorthWest | West | SouthWest | South | SouthEast
  deriving (Eq, Ord, Show)

data Pair = Pair !Ix2 !Direction

-- | Focal Length - the length of the lineal structure at every location. The result is in
-- "pixel units", where 1 is the height/width of one pixel.
flength :: Manifest u Ix2 (S.Set Direction) => Raster u p r c (S.Set Direction) -> Raster DW p r c Double
flength (Raster a) = Raster $ mapStencil lenStencil a

lenStencil :: Stencil Ix2 (S.Set Direction) Double
lenStencil = makeStencil (Fill mempty) (3 :. 3) (1 :. 1) $ \f ->
  fmap (work . M.fromList) $ P.traverse (\i -> sequenceA (i, f i)) ixs
  where ixs  = (:.) <$> [-1 .. 1] <*> [-1 .. 1]
{-# INLINE lenStencil #-}

work :: M.Map Ix2 (S.Set Direction) -> Double
work m = maybe 0 id $ do
  focus <- M.lookup (0 :. 0) m
  axes  <- P.traverse (\d -> let i = advance d in (i,) <$> M.lookup i m) $ S.toList focus
  let corners  = S.delete (0 :. 0) . mconcat $ P.map (\(i,ds) -> S.map ((+) i . advance) ds) axes
      cornDirs = mapMaybe (`M.lookup` m) $ S.toList corners
      dirs     = focus : (P.map snd axes <> cornDirs)
  pure $ foldl' (\acc s -> acc + S.foldl' (\acc' d -> acc' + g d) 0 s) 0 dirs
  where half = 1 / 2
        root = 1 / sqrt 2
        g North = half
        g West  = half
        g South = half
        g East  = half
        g NorthWest = root
        g SouthWest = root
        g SouthEast = root
        g NorthEast = root

-- | A delta that can be applied to other `Ix2` to "move" in the direction given.
advance :: Direction -> Ix2
advance North = (-1 :. 0)
advance West  = (0  :. -1)
advance South = (1  :. 0)
advance East  = (0  :. 1)
advance NorthWest = (-1 :. -1)
advance SouthWest = (1  :. -1)
advance SouthEast = (1  :. 1)
advance NorthEast = (-1 :. 1)

-- | A pixel of a `Raster` with areal information about its corners.
data Cell a = Cell { _cell :: !a, _corners :: !(Corners a) } deriving (Eq, Show)

instance Default a => Default (Cell a) where
  def = Cell def (Corners Open Open Open Open)
  {-# INLINE def #-}

-- | A layout of the areal conditions of a single `Raster` pixel.
-- It describes whether each pixel corner is occupied by the same
-- "areal zone" as the pixel centre.
data Corners a = Corners { _topLeft     :: !(Surround a)
                         , _bottomLeft  :: !(Surround a)
                         , _bottomRight :: !(Surround a)
                         , _topRight    :: !(Surround a) } deriving (Eq, Show)

-- | A state of surroundedness of a pixel corner.
-- For the examples below, the bottom-left pixel is considered the focus and
-- we're wondering about the surroundedness of its top-right corner.
data Surround a = Complete !a  -- ^ A corner has three of the same opponent against it.
                               --
                               -- The corner is considered "occupied" by the opponent value,
                               -- thus forming a diagonal areal edge.
                               --
                               -- @
                               -- [ 1 1 ]
                               -- [ 0 1 ]
                               -- @
                | OneSide      -- ^ One edge of a corner is touching an opponent, but
                               -- the other edge touches a friend.
                               --
                               -- @
                               -- [ 1 1 ]  or  [ 0 1 ]
                               -- [ 0 0 ]      [ 0 1 ]
                               -- @
                | Open         -- ^ A corner is surrounded by friends.
                               --
                               -- @
                               -- [ 0 0 ]  or  [ 0 0 ]  or  [ 1 0 ]
                               -- [ 0 0 ]      [ 0 1 ]      [ 0 0 ]
                               -- @
                | RightAngle   -- ^ Similar to `Complete`, except that the diagonal
                               -- opponent doesn't match the other two. The corner
                               -- is considered surrounded, but not "occupied".
                               --
                               -- @
                               -- [ 1 2 ]
                               -- [ 0 1 ]
                               -- @
  deriving (Eq, Ord, Show)

-- | Imagining a 2x2 neighbourhood with its focus in the bottom-left,
-- what `Surround` relationship does the focus have with the other pixels?
surround :: Eq a => a -> a -> a -> a -> (Surround a)
surround fo tl tr br
  | up && tl == tr && tr == br = Complete tr
  | up && right = RightAngle
  | (up && diag) || (diag && right) = OneSide
  | otherwise = Open
  where up    = fo /= tl
        diag  = fo /= tr
        right = fo /= br
{-# INLINE surround #-}

-- | What is the total length of the areal edges (if there are any) at a given pixel?
frontage :: Corners a -> Double
frontage (Corners tl bl br tr) = f tl + f bl + f br + f tr
  where f (Complete _) = 1 / sqrt 2
        f OneSide      = 1 / 2
        f Open         = 0
        f RightAngle   = 1

-- | If the given pixel is not the neighbourhood focus nor does it share its value with the focus,
-- it might still have some of the focus's area incurring. In this case,
-- only `Complete` would contribute to the focus's areal frontage.
frontage' :: Eq a => a -> Corners a -> Double
frontage' a (Corners tl bl br tr) = f tl + f bl + f br + f tr
  where f (Complete a') = bool 0 (1 / sqrt 2) $ a == a'
        f _ = 0
{-# INLINE frontage' #-}

-- | Focal Partition - the areal form of each location, only considering
-- the top-right edge.
fpartition :: (Default a, Eq a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c (Cell a)
fpartition (Raster a) = Raster $ mapStencil partStencil a

partStencil :: (Eq a, Default a) => Stencil Ix2 a (Cell a)
partStencil = makeStencil Reflect (2 :. 2) (1 :. 0) $ \f -> do
  tl <- f (-1 :. 0)
  tr <- f (-1 :. 1)
  br <- f (0  :. 1)
  fo <- f (0  :. 0)
  pure $ Cell fo $ Corners (surround fo tl tl fo) Open (surround fo fo br br) (surround fo tl tr br)
{-# INLINE partStencil #-}

-- | Like `fpartition`, but considers the `Surround` of all corners. Is alluded to
-- in GaCM but isn't given its own operation name.
--
-- If preparing for `ffrontage` or `farea`, you almost certainly want this function and
-- not `fpartition`.
fshape :: (Default a, Eq a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c (Cell a)
fshape (Raster a) = Raster $ mapStencil arealsStencil a

arealsStencil :: (Eq a, Default a) => Stencil Ix2 a (Cell a)
arealsStencil = makeStencil Reflect (3 :. 3) (1 :. 1) $ \f -> do
  tl <- f (-1 :. -1)
  up <- f (-1 :. 0)
  tr <- f (-1 :. 1)
  le <- f (0  :. -1)
  fo <- f (0  :. 0)
  ri <- f (0  :. 1)
  bl <- f (1  :. -1)
  bo <- f (1  :. 0)
  br <- f (1  :. 1)
  pure $ Cell fo $ Corners (surround fo up tl le)
                           (surround fo bo bl le)
                           (surround fo bo br ri)
                           (surround fo up tr ri)
{-# INLINE arealsStencil #-}

-- | Focal Frontage - the length of areal edges between each pixel and its neighbourhood.
--
-- Usually, the output of `fshape` is the appropriate input for this function.
ffrontage :: (Eq a, Default a, Manifest u Ix2 (Cell a)) => Raster u p r c (Cell a) -> Raster DW p r c Double
ffrontage (Raster a) = Raster $ mapStencil (percStencil f Reflect) a
  where f (Cell fo cs) vs = frontage cs + foldl' (\acc (Cell v cs') -> acc + (bool (frontage' fo cs') (frontage cs') $ v == fo)) 0 vs

-- | The area of a 1x1 square is 1. It has 8 right-triangular sections,
-- each with area 1/8. So, here we prefer integer math for its speed,
-- only diving by 8 a final time at the end, back in `farea`.
area :: Corners a -> Int
area (Corners tl bl br tr) = 8 - f tl - f bl - f br - f tr
  where f (Complete _) = 1
        f _ = 0
{-# INLINE area #-}

area' :: Eq a => a -> Corners a -> Int
area' fo (Corners tl bl br tr) = f tl + f bl + f br + f tr
  where f (Complete a) | a == fo = 1
        f _ = 0
{-# INLINE area' #-}

-- | Focal Area - the area of the shape made up by a neighbourhood focus and its
-- surrounding pixels. Each pixel is assumed to have length and width of 1.
--
-- Usually, the output of `fshape` is the appropriate input for this function.
farea :: (Eq a, Default a, Manifest u Ix2 (Cell a)) => Raster u p r c (Cell a) -> Raster DW p r c Double
farea (Raster a) = Raster $ mapStencil (percStencil f Reflect) a
  where f (Cell fo cs) vs = fromIntegral (area cs + foldl' (\acc (Cell v cs') -> acc + (bool (area' fo cs') (area cs') $ v == fo)) 0 vs) / 8

-- | Focal Volume - the surficial volume under each pixel, assuming the `Raster`
-- represents elevation in some way.
fvolume :: (Fractional a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fvolume (Raster a) = Raster $ mapStencil volStencil a

volStencil :: (Fractional a, Default a) => Stencil Ix2 a a
volStencil = makeStencil Reflect (3 :. 3) (1 :. 1) $ \f -> do
  tl <- f (-1 :. -1)
  up <- f (-1 :. 0)
  tr <- f (-1 :. 1)
  le <- f (0  :. -1)
  fo <- f (0  :. 0)
  ri <- f (0  :. 1)
  bl <- f (1  :. -1)
  bo <- f (1  :. 0)
  br <- f (1  :. 1)
  pure $
    let nw = (tl + up + le + fo) / 4
        no = (up + fo) / 2
        ne = (up + tr + fo + ri) / 4
        we = (le + fo) / 2
        ea = (fo + ri) / 2
        sw = (le + fo + bl + bo) / 4
        so = (fo + bo) / 2
        se = (fo + ri + bo + br) / 4
    in volume fo nw no
       + volume fo no ne
       + volume fo ne ea
       + volume fo ea se
       + volume fo se so
       + volume fo so sw
       + volume fo sw we
       + volume fo we nw  -- TODO Optimize this by removing `volume` and dividing by 24 only once at the end.
{-# INLINE volStencil #-}

-- | Given three points that form a triangle in some 3-dimensional vector
-- space, calculate the volume of the prism it forms if projected onto
-- the @z = 0@ plane. Negative volume is possible.
volume :: Fractional a => a -> a -> a -> a
volume a b c = (a + b + c) / 24
{-# INLINE volume #-}

-- | The first part to the "left pseudo inverse" needed to calculate
-- a best-fitting plane of 9 points.
leftPseudo :: LA.Matrix Double
leftPseudo = LA.inv (aT <> a) <> aT
  where aT = LA.tr' a
        a  = LA.matrix 3 [ -1, -1, 1
                         , -1, 0, 1
                         , -1, 1, 1
                         , 0, -1, 1
                         , 0, 0, 1
                         , 0, 1, 1
                         , 1, -1, 1
                         , 1, 0, 1
                         , 1, 1, 1 ]

-- TODO: newtype wrapper for `Radians`?
-- | Focal Gradient - a measurement of surficial slope for each pixel relative to
-- the horizonal cartographic plane. Results are in radians, with a flat plane
-- having a slope angle of 0 and a near-vertical plane approaching \(\tau / 4\).
fgradient :: Manifest u Ix2 Double => Raster u p r c Double -> Raster DW p r c Double
fgradient (Raster a) = Raster $ mapStencil (groupStencil gradient Reflect) a

-- | One full rotation of the unit circle.
tau :: Double
tau = 6.283185307179586

-- | Given a list of \(z\) values, find the slope of the best-fit
-- plane that matches those points.
--
-- See: https://stackoverflow.com/a/16669463/643684
gradient :: [Double] -> Double
gradient vs = (tau / 2) - (acos $ normal vs LA.! 2)

-- | Given a list of \(z\) values, find a normal vector that /points down/
-- from a best-fit plane toward the cartographic plane.
normal :: [Double] -> LA.Vector Double
normal = LA.normalize . zcoord (-1) . normal'

-- | A non-normalized, non-Z-corrected normal. Handy for `faspect`,
-- which needs to drop the Z and renormalize.
normal' :: [Double] -> LA.Vector Double
normal' vs = leftPseudo LA.#> LA.vector vs

-- | Replace the Z-coordinate of a Vector.
zcoord :: Double -> LA.Vector Double -> LA.Vector Double
zcoord n v = LA.vector [ v LA.! 0, v LA.! 1, n ]

-- | Focal Aspect - the compass direction toward which the surface
-- descends most rapidly. Results are in radians, with TODO (talk about directions)
faspect :: Manifest u Ix2 Double => Raster u p r c Double -> Raster DW p r c (Maybe Double)
faspect (Raster a) = Raster $ mapStencil (groupStencil f Reflect) a
  where f vs = case normal' vs of
                 n | ((n LA.! 0) =~ 0) && ((n LA.! 1) =~ 0) -> Nothing
                   | otherwise -> Just . acos $ LA.dot (LA.normalize $ zcoord 0 n) axis
        axis = LA.vector [1, 0, 0]

-- | Approximate Equality.
(=~) :: Double -> Double -> Bool
a =~ b = abs (a - b) < 0.0001

-- TODO faspect', the (likely) faster but unrigourous version that gives undefined
-- results when the normal is straight down.
