{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module    : Geography.MapAlgebra
-- Copyright : (c) Colin Woodbury, 2018 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- This library is an implementation of /Map Algebra/ as described in the book
-- /GIS and Cartographic Modeling/ (GaCM) by Dana Tomlin. The fundamental
-- primitive is the `Raster`, a rectangular grid of data that usually describes
-- some area on the earth. A `Raster` need not contain numerical data, however,
-- and need not just represent satellite imagery. It is essentially a matrix,
-- which of course forms a `Functor`, and thus is available for all the
-- operations we would expect to run on any Functor. /GIS and Cartographic
-- Modeling/ doesn't lean on this fact, and so describes many seemingly custom
-- operations which to Haskell are just applications of `fmap` or `zipWith` with
-- pure functions.
--
-- Here are the main classes of operations ascribed to /Map Algebra/ and their
-- corresponding approach in Haskell:
--
-- * Single-Raster /Local Operations/ -> `fmap` with pure functions
-- * Multi-Raster /Local Operations/ -> `foldl` with `zipWith` and pure functions
-- * /Focal Operations/ -> 'massiv'-based smart `Stencil` operations
-- * /Zonal Operations/ -> Not yet implemented
--
-- Whether it is meaningful to perform operations between two given `Raster`s
-- (i.e. whether the Rasters properly overlap on the earth) is not handled in
-- this library and is left to the application.
--
-- The "colour ramp" generation functions (like `greenRed`) gratefully borrow
-- colour sets from Gretchen N. Peterson's book /Cartographer's Toolkit/.
--
-- === A Word on Massiv: Fused, Parallel Arrays
-- Thanks to the underlying `Array` library
-- [massiv](https://hackage.haskell.org/package/massiv), most operations over
-- and between Rasters are /fused/, meaning that no extra memory is allocated in
-- between each step of a composite operation.
--
-- Take the [Enhanced Vegetation Index](https://en.wikipedia.org/wiki/Enhanced_vegetation_index)
-- calculation:
--
-- @
-- evi :: Raster D p r c Double -> Raster D p r c Double -> Raster D p r c Double -> Raster D p r c Double
-- evi nir red blue = 2.5 * (numer / denom)
--   where numer = nir - red
--         denom = nir + (6 * red) - (7.5 * blue) + 1
-- @
--
-- 8 binary operators are used here, but none allocate new memory. It's only
-- when some `lazy` Raster is made `strict` that calculations occur and memory
-- is allocated.
--
-- Provided your machine has more than 1 CPU, Rasters read by functions like
-- `fromRGBA` will automatically be in `Par`allel mode. This means that forcing
-- calculations with `strict` will cause evaluation to be done with every CPU
-- your machine has. The effect of this is quite potent for Focal Operations,
-- which yield special, cache-friendly windowed (`DW`) Rasters.
--
-- Familiarity with Massiv will help in using this library. A guide
-- [can be found here](https://github.com/lehins/massiv).
--
-- === Compilation Options for Best Performance
-- When using this library, always compile your project with @-threaded@ and
-- @-with-rtsopts=-N@. These will ensure your executables will automatically use
-- all the available CPU cores.
--
-- As always, @-O2@ is your friend. The @{-\# INLINE ... \#-}@ pragma is also
-- very likely to improve the performance of code that uses functions from this
-- library. Make sure to benchmark proactively.
--
-- For particularly mathy operations like `fmean`, compiling with @-fllvm@
-- grants about a 2x speedup.

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
  -- | To colour a `Raster`:
  --
  -- 1. Fully evaluate it with `strict` `S`.
  -- 2. Use `histogram` to analyse the spread of its values. Currently only `Word8` is supported.
  -- 3. Use `breaks` on the `Histogram` to generate a list of "break points".
  -- 4. Pass the break points to a function like `greenRed` to generate a "colour ramp" (a `M.Map`).
  -- 5. Use this ramp with `classify` to colour your `Raster` in \(\mathcal{O}(n\log(n))\).
  --    `invisible` can be used as the default colour for values that fall outside the range
  --    of the Map.
  --
  -- @
  -- colourIt :: Raster D p r c Word8 -> Raster S p r c (Pixel RGBA Word8)
  -- colourIt (strict S -> r) = strict S . classify invisible cm $ lazy r
  --   where cm = blueGreen . breaks $ histogram r
  -- @
  --
  -- If you aren't interested in colour but still want to render your `Raster`,
  -- consider `grayscale`. Coloured `Raster`s can be unwrapped with `_array` and
  -- then output with functions like `writeImage`.
  , grayscale
  , Histogram(..), histogram, breaks
  , invisible
  , greenRed, spectrum, blueGreen, purpleYellow, brownBlue
  , grayBrown, greenPurple, brownYellow, purpleGreen, purpleRed
  -- *** Output and Display
  -- | For coloured output, first use `classify` over your `Raster` to produce a
  -- @Raster u p r c (Pixel RGBA Word8)@. Then unwrap it with `_array` and output
  -- with something like `writeImage`.
  --
  -- For quick debugging, you can visualize a `Raster` with `displayImage`:
  --
  -- @
  -- raster :: Raster D p 512 512 Word8
  --
  -- >>> displayImage . _array . strict S . grayscale
  -- @
  , writeImage, writeImageAuto
  , displayImage
  , png
  -- ** Projections
  , Projection(..)
  , reproject
  , Sphere, LatLng, WebMercator
  , Point(..)
  -- * Map Algebra
  -- ** Local Operations
  -- | Operations between `Raster`s. All operations are element-wise:
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
  -- zipWith :: (a -> b -> d) -> Raster u p r c a -> Raster u p r c b -> Raster D p r c d
  --
  -- -- Your operation.
  -- foo :: Int -> Int -> Int
  --
  -- bar :: Raster u p r c Int -> Raster u p r c Int -> Raster D p r c Int
  -- bar a b = zipWith foo a b
  -- @
  , zipWith
  -- *** Unary
  -- | If you want to do simple unary @Raster -> Raster@ operations (called
  -- /LocalCalculation/ in GaCM), `Raster` is a `Functor` so you can use `fmap`
  -- as normal:
  --
  -- @
  -- myRaster :: Raster D p r c Int
  --
  -- -- Add 17 to every value in the Raster.
  -- fmap (+ 17) myRaster
  -- @
  , classify
  -- *** Binary
  -- | You can safely use these with the `foldl` family on any `Foldable` of
  -- `Raster`s. You would likely want @foldl1'@ which is provided by both List
  -- and Vector.
  --
  -- Keep in mind that `Raster` `D` has a `Num` instance, so you can use all the
  -- normal math operators with them as well.
  , lmax, lmin
  -- *** Other
  -- | There is no binary form of these functions that exists without
  -- producing numerical error,  so you can't use the `foldl` family with these.
  -- Consider the average operation, where the following is /not/ true:
  -- \[
  --    \forall abc \in \mathbb{R}. \frac{\frac{a + b}{2} + c}{2} = \frac{a + b + c}{3}
  -- \]
  , lmean, lvariety, lmajority, lminority, lvariance
  -- ** Focal Operations
  -- | Operations on one `Raster`, given some polygonal neighbourhood. Your
  -- `Raster` must be of a `Manifest` type (i.e. backed by real memory) before
  -- you attempt any focal operations. Without this constraint, wayward users
  -- run the risk of setting up operations that would perform terribly. Use
  -- `strict` to easily convert a lazy `Raster` to a memory-backed one.
  --
  -- @
  -- myRaster :: Raster D p r c Float
  --
  -- averaged :: Raster DW p r c Float
  -- averaged = fmean $ strict P myRaster
  -- @
  , fsum, fproduct, fmonoid, fmean
  , fmax, fmin
  , fmajority, fminority, fvariety
  , fpercentage, fpercentile
  -- *** Lineal
  -- | Focal operations that assume that groups of data points represent
  -- line-like objects in a `Raster`. GaCM calls these /lineal characteristics/
  -- and describes them fully on page 18 and 19.
  , Line(..)
  , flinkage, flength
  -- *** Areal
  -- | Focal operations that assume that groups of data points represent 2D
  -- areas in a `Raster`. GaCM calls these /areal characteristics/ and describes
  -- them fully on page 20 and 21.
  , Corners(..), Surround(..)
  , fpartition, fshape, ffrontage, farea
  -- *** Surficial
  -- | Focal operations that work over elevation `Raster`s. GaCM calls elevation
  -- features /surficial characteristics/ and describes them fully on page 21
  -- and 22.
  --
  -- Some of these operations require finding a "best-fit plane" that
  -- approximates the surficial shape of each pixel. Each pixel has 9 "facet
  -- points" calculated for it based on its surrounding pixels. We then use
  -- these facets to determine a plane which adheres to this equation:
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
  -- The boxed section is called the "left pseudo inverse" and is available as
  -- `leftPseudo`. The actual values of \(A\) don't matter for our purposes,
  -- hence \(A\) can be fixed to avoid redundant calculations.
  , Drain(..), Direction(..)
  , direction, directions, drainage
  , fvolume, fgradient, faspect, faspect', fdownstream, fupstream
  -- * Utilities
  , leftPseudo, tau
  -- * Massiv Re-exports
  -- | For your convenience.
  , D(..), DW(..), S(..), P(..), U(..), B(..), N(..)
  , Ix2(..), Comp(..)
  , Pixel(..), RGBA, Y
  ) where

import           Control.Concurrent (getNumCapabilities)
import           Control.DeepSeq (NFData(..), deepseq)
import           Control.Monad.ST
import           Data.Bits (testBit)
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Data.Massiv.Array hiding (zipWith)
import qualified Data.Massiv.Array as A
import           Data.Massiv.Array.IO
import qualified Data.Massiv.Array.Manifest.Vector as A
import           Data.Massiv.Array.Unsafe as A
import           Data.Proxy (Proxy(..))
import           Data.Semigroup
import qualified Data.Set as S
import           Data.Typeable (Typeable)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Data.Word
import           GHC.TypeLits
import           Graphics.ColorSpace (ColorSpace, Elevator, Pixel(..), RGBA, Y)
import qualified Numeric.LinearAlgebra as LA
import           Prelude hiding (zipWith)
import qualified Prelude as P
import           Text.Printf (printf)

--

-- | A location on the Earth in some `Projection`.
data Point p = Point { x :: !Double, y :: !Double } deriving (Eq, Show)

-- | The Earth is not a sphere. Various schemes have been invented throughout
-- history that provide `Point` coordinates for locations on the earth, although
-- all are approximations and come with trade-offs. We call these "Projections",
-- since they are a mapping of `Sphere` coordinates to some other approximation.
-- The Projection used most commonly for mapping on the internet is called
-- `WebMercator`.
--
-- A Projection is also known as a Coordinate Reference System (CRS).
--
-- Use `reproject` to convert `Point`s between various Projections.
--
-- __Note:__ Full support for Projections is still pending.
class Projection p where
  -- | Convert a `Point` in this Projection to one of radians on a perfect `Sphere`.
  toSphere :: Point p -> Point Sphere

  -- | Convert a `Point` of radians on a perfect sphere to that of a specific Projection.
  fromSphere :: Point Sphere -> Point p

-- | Reproject a `Point` from one `Projection` to another.
reproject :: (Projection p, Projection r) => Point p -> Point r
reproject = fromSphere . toSphere
{-# INLINE reproject #-}

-- | A perfect geometric sphere. The earth isn't actually shaped this way, but
-- it's a convenient middle-ground for converting between various `Projection`s.
data Sphere

instance Projection Sphere where
  toSphere = id
  fromSphere = id

-- | Latitude (north-south position) and Longitude (east-west position).
data LatLng

-- instance Projection LatLng where
--   toSphere = undefined
--   fromSphere = undefined

-- | The most commonly used `Projection` for mapping in internet applications.
data WebMercator

-- instance Projection WebMercator where
--   toSphere = undefined
--   fromSphere = undefined

-- | A rectangular grid of data representing some area on the earth.
--
-- * @u@: What is the /underlying representation/ of this Raster? (see 'massiv')
-- * @p@: What `Projection` is this Raster in?
-- * @r@: How many rows does this Raster have?
-- * @c@: How many columns does this Raster have?
-- * @a@: What data type is held in this Raster?
--
-- By having explicit p, r, and c, we make impossible any operation between two
-- Rasters of differing size or projection. Conceptually, we consider Rasters of
-- different size and projection to be /entirely different types/. Example:
--
-- @
-- -- | A lazy 256x256 Raster with the value 5 at every index. Uses DataKinds
-- -- and "type literals" to achieve the same-size guarantee.
-- myRaster :: Raster D WebMercator 256 256 Int
-- myRaster = constant D Par 5
--
-- >>> length myRaster
-- 65536
-- @
newtype Raster u p (r :: Nat) (c :: Nat) a = Raster { _array :: Array u Ix2 a }

-- | Warning: This will evaluate (at most) the 10x10 top-left corner of your
-- `Raster` for display. This should only be used for debugging.
instance (Show a, Load (R u) Ix2 a, Extract u Ix2 a) => Show (Raster u p r c a) where
  show (Raster a) = show . computeAs B $ extract' zeroIndex (Sz2 (P.min 10 r) (P.min 10 c)) a
    where Sz (r :. c) = size a

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
  {-# INLINE abs #-}

  signum = fmap signum
  {-# INLINE signum #-}

  fromInteger = constant D Par . fromInteger
  {-# INLINE fromInteger #-}

instance (Fractional a, KnownNat r, KnownNat c) => Fractional (Raster D p r c a) where
  a / b = zipWith (/) a b
  {-# INLINE (/) #-}

  fromRational = constant D Par . fromRational
  {-# INLINE fromRational #-}

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
  length (Raster a) = (\(Sz2 r c) -> r * c) $ A.size a
  {-# INLINE length #-}

-- | \(\mathcal{O}(1)\). Force a `Raster`'s representation to `D`, allowing it
-- to undergo various operations. All operations between `D` `Raster`s are fused
-- and allocate no extra memory.
lazy :: Source u Ix2 a => Raster u p r c a -> Raster D p r c a
lazy (Raster a) = Raster $ delay a
{-# INLINE lazy #-}

-- | Evaluate some lazy (`D`, `DW`, or `DI`) `Raster` to some explicit
-- `Manifest` type (i.e. to a real memory-backed Array). Will follow the
-- `Comp`utation strategy of the underlying 'massiv' `Array`.
--
-- __Note:__ If using the `Par` computation strategy, make sure you're compiling
-- with @-with-rtsopts=-N@ to automatically use all available CPU cores at
-- runtime. Otherwise your "parallel" operations will only execute on one core.
strict :: (Load v Ix2 a, Mutable u Ix2 a) => u -> Raster v p r c a -> Raster u p r c a
strict u (Raster a) = Raster $ computeAs u a
{-# INLINE strict #-}

-- | Create a `Raster` of any size which has the same value everywhere.
constant :: (KnownNat r, KnownNat c, Construct u Ix2 a) => u -> Comp -> a -> Raster u p r c a
constant u c a = fromFunction u c (const a)
{-# INLINE constant #-}

-- | \(\mathcal{O}(1)\). Create a `Raster` from a function of its row and column
-- number respectively.
fromFunction :: forall u p r c a. (KnownNat r, KnownNat c, Construct u Ix2 a) =>
  u -> Comp -> (Ix2 -> a) -> Raster u p r c a
fromFunction u c f = Raster $ makeArrayR u c (Sz sh) f
  where sh = fromInteger (natVal (Proxy :: Proxy r)) :. fromInteger (natVal (Proxy :: Proxy c))
{-# INLINE fromFunction #-}

-- | \(\mathcal{O}(1)\). Create a `Raster` from the values of any `GV.Vector`
-- type. Will fail if the size of the Vector does not match the declared size of
-- the `Raster`.
fromVector :: forall v p r c a. (KnownNat r, KnownNat c, GV.Vector v a, Mutable (A.ARepr v) Ix2 a, Typeable v) =>
  Comp -> v a -> Either String (Raster (A.ARepr v) p r c a)
-- fromVector comp v | (r * c) == GV.length v = Right . Raster $ A.fromVector comp (r :. c) v
fromVector comp v | (r * c) == GV.length v = Right . Raster $ A.fromVector' comp (Sz2 r c) v
                  | otherwise = Left $ printf "Expected Pixel Count: %d - Actual: %d" (r * c) (GV.length v)
  where r = fromInteger $ natVal (Proxy :: Proxy r)
        c = fromInteger $ natVal (Proxy :: Proxy c)
{-# INLINE fromVector #-}

-- | An RGBA image whose colour bands are distinct.
data RGBARaster p r c a = RGBARaster { _red   :: !(Raster S p r c a)
                                     , _green :: !(Raster S p r c a)
                                     , _blue  :: !(Raster S p r c a)
                                     , _alpha :: !(Raster S p r c a) }

-- | Read any image type into a `Raster` of distinct colour bands with the cell
-- type you declare. If the source image stores its values as `Int` but you
-- declare `Double`, the conversion will happen automatically.
--
-- Will fail if the declared size of the `Raster` does not match the actual size
-- of the input image.
fromRGBA :: forall p r c a. (Elevator a, KnownNat r, KnownNat c) => FilePath -> IO (Either String (RGBARaster p r c a))
fromRGBA fp = do
  cap <- getNumCapabilities
  img <- setComp (bool Par Seq $ cap == 1) <$> readImageAuto fp
  let rows = fromInteger $ natVal (Proxy :: Proxy r)
      cols = fromInteger $ natVal (Proxy :: Proxy c)
      Sz (r :. c) = size img
  if r == rows && c == cols
    then do
    (ar, ag, ab, aa) <- spreadRGBA img
    pure . Right $ RGBARaster (Raster ar) (Raster ag) (Raster ab) (Raster aa)
    else pure . Left $ printf "Expected Size: %d x %d - Actual Size: %d x %d" rows cols r c
{-# INLINE fromRGBA #-}

spreadRGBA :: (Index ix, Elevator e)
  => A.Array S ix (Pixel RGBA e)
  -> IO (A.Array S ix e, A.Array S ix e, A.Array S ix e, A.Array S ix e)
spreadRGBA arr = do
  let sz = A.size arr
  mr <- A.unsafeNew sz
  mb <- A.unsafeNew sz
  mg <- A.unsafeNew sz
  ma <- A.unsafeNew sz
  flip A.imapM_ arr $ \i (PixelRGBA r g b a) -> do
    A.unsafeWrite mr i r
    A.unsafeWrite mg i g
    A.unsafeWrite mb i b
    A.unsafeWrite ma i a
  ar <- A.unsafeFreeze (getComp arr) mr
  ag <- A.unsafeFreeze (getComp arr) mg
  ab <- A.unsafeFreeze (getComp arr) mb
  aa <- A.unsafeFreeze (getComp arr) ma
  return (ar, ag, ab, aa)
{-# INLINE spreadRGBA #-}

-- | Read a grayscale image. If the source file has more than one colour band,
-- they'll be combined automatically.
fromGray :: forall p r c a. (Elevator a, KnownNat r, KnownNat c) => FilePath -> IO (Either String (Raster S p r c a))
fromGray fp = do
  cap <- getNumCapabilities
  img <- setComp (bool Par Seq $ cap == 1) <$> readImageAuto fp
  let rows = fromInteger $ natVal (Proxy :: Proxy r)
      cols = fromInteger $ natVal (Proxy :: Proxy c)
      Sz (r :. c) = size img
  pure . bool (Left $ printf "Expected Size: %d x %d - Actual Size: %d x %d" rows cols r c) (Right $ f img) $ r == rows && c == cols
  where f :: Image S Y a -> Raster S p r c a
        f img = Raster . A.fromVector' (getComp img) (size img) . VS.unsafeCast $ A.toVector img
{-# INLINE fromGray #-}

-- | An invisible pixel (alpha channel set to 0).
invisible :: Pixel RGBA Word8
invisible = PixelRGBA 0 0 0 0

-- | Construct a colour ramp.
-- ramp :: Ord k => [(Word8, Word8, Word8)] -> [k] -> M.Map k PixelRGBA8
ramp :: Ord k => [(Word8, Word8, Word8)] -> [k] -> M.Map k (Pixel RGBA Word8)
ramp colours bs = M.fromList . P.zip bs $ P.map (\(r,g,b) -> PixelRGBA r g b maxBound) colours
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

-- | Convert a `Raster` into grayscale pixels, suitable for easy output with
-- functions like `writeImage`.
grayscale :: Functor (Raster u p r c) => Raster u p r c a -> Raster u p r c (Pixel Y a)
grayscale = fmap PixelY
{-# INLINE grayscale #-}

-- | Render a PNG-encoded `BL.ByteString` from a coloured `Raster`. Useful for
-- returning a `Raster` from a webserver endpoint.
png :: (Source u Ix2 (Pixel cs a), ColorSpace cs a) => Raster u p r c (Pixel cs a) -> BL.ByteString
png (Raster a) = encode PNG def a
{-# INLINE png #-}

-- | Called /LocalClassification/ in GaCM. The first argument is the value to
-- give to any index whose value is less than the lowest break in the `M.Map`.
--
-- This is a glorified `fmap` operation, but we expose it for convenience.
classify :: (Ord a, Functor f) => b -> M.Map a b -> f a -> f b
classify d m r = fmap f r
  where f a = maybe d snd $ M.lookupLE a m
{-# INLINE classify #-}

-- | Finds the minimum value at each index between two `Raster`s.
lmin :: (Ord a, Source u Ix2 a) => Raster u p r c a -> Raster u p r c a -> Raster D p r c a
lmin = zipWith P.min
{-# INLINE lmin #-}

-- | Finds the maximum value at each index between two `Raster`s.
lmax :: (Ord a, Source u Ix2 a) => Raster u p r c a -> Raster u p r c a -> Raster D p r c a
lmax = zipWith P.max
{-# INLINE lmax #-}

-- | Averages the values per-index of all `Raster`s in a collection.
lmean :: (Real a, Fractional b, KnownNat r, KnownNat c) => NonEmpty (Raster D p r c a) -> Raster D p r c b
lmean (a :| [b])   = Raster $ A.zipWith (\n m -> realToFrac (n + m) / 2) (_array a) (_array b)
lmean (a :| [b,c]) = Raster $ A.zipWith3 (\n m o -> realToFrac (n + m + o) / 3) (_array a) (_array b) (_array c)
lmean (a :| as)    = (\n -> realToFrac n / len) <$> foldl' (+) a as
  where len = 1 + fromIntegral (length as)
{-# INLINE lmean #-}

-- | The count of unique values at each shared index.
lvariety :: (KnownNat r, KnownNat c, Eq a) => NonEmpty (Raster D p r c a) -> Raster D p r c Word
lvariety = fmap (fromIntegral . length . NE.nub) . P.sequenceA
{-# INLINE lvariety #-}

-- | The most frequently appearing value at each shared index.
lmajority :: (KnownNat r, KnownNat c, Ord a) => NonEmpty (Raster D p r c a) -> Raster D p r c a
lmajority = fmap majo . P.sequenceA
{-# INLINE lmajority #-}

-- | Find the most common value in some `Foldable`.
majo :: forall t a. (Foldable t, Ord a) => t a -> a
majo = fst . g . f
  where
    f :: t a -> M.Map a Int
    f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty

    g :: M.Map a Int -> (a, Int)
    g = L.foldl1' (\(a,c) (k,v) -> if c < v then (k,v) else (a,c)) . M.toList
{-# INLINE majo #-}

-- | The least frequently appearing value at each shared index.
lminority :: (KnownNat r, KnownNat c, Ord a) => NonEmpty (Raster D p r c a) -> Raster D p r c a
lminority = fmap mino . P.sequenceA
{-# INLINE lminority #-}

-- | Find the least common value in some `Foldable`.
mino :: forall t a. (Foldable t, Ord a) => t a -> a
mino = fst . g . f
  where
    f :: t a -> M.Map a Int
    f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty

    g :: M.Map a Int -> (a, Int)
    g = L.foldl1' (\(a,c) (k,v) -> if c > v then (k,v) else (a,c)) . M.toList
{-# INLINE mino #-}

-- | A measure of how spread out a dataset is. This calculation will fail with
-- `Nothing` if a length 1 list is given.
lvariance
  :: forall p a r c. (Real a, KnownNat r, KnownNat c)
  => NonEmpty (Raster D p r c a) -> Maybe (Raster D p r c Double)
lvariance (_ :| []) = Nothing
lvariance rs = Just (f <$> P.sequenceA rs)
  where
    len :: Double
    len = realToFrac $ length rs

    avg :: NonEmpty a -> Double
    avg ns = (\z -> realToFrac z / len) $ foldl' (+) 0 ns

    f :: NonEmpty a -> Double
    f os@(n :| ns) = num / (len - 1)
      where
        num = foldl' (\acc m -> acc + ((realToFrac m - av) ^ (2 :: Int))) ((realToFrac n - av) ^ (2 :: Int)) ns
        av = avg os
{-# INLINE lvariance #-}

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

-- | Focal Addition.
fsum :: (Num a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fsum (Raster a) = Raster $ mapStencil (Fill 0) (sumStencil $ Sz2 3 3) a
{-# INLINE fsum #-}

-- | Focal Product.
fproduct :: (Num a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fproduct (Raster a) = Raster $ mapStencil (Fill 1) (productStencil $ Sz2 3 3) a
{-# INLINE fproduct #-}

-- | Focal Monoid - combine all elements of a neighbourhood via their `Monoid`
-- instance. In terms of precedence, the neighbourhood focus is the "left-most",
-- and all other elements are "added" to it.
--
-- This is not mentioned in GaCM, but seems useful nonetheless.
fmonoid :: (Monoid a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmonoid (Raster a) = Raster $ mapStencil (Fill mempty) (foldStencil $ Sz2 3 3) a
{-# INLINE fmonoid #-}

-- | Focal Mean.
fmean :: (Fractional a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmean (Raster a) = Raster $ mapStencil (Fill 0) (avgStencil $ Sz2 3 3) a
{-# INLINE fmean #-}

-- | Focal Maximum.
fmax :: (Ord a, Bounded a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmax (Raster a) = Raster $ mapStencil Edge (maxStencil $ Sz2 3 3) a
{-# INLINE fmax #-}

-- | Focal Minimum.
fmin :: (Ord a, Bounded a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmin (Raster a) = Raster $ mapStencil Edge (minStencil $ Sz2 3 3) a
{-# INLINE fmin #-}

-- | Focal Variety - the number of unique values in each neighbourhood.
fvariety :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Word
fvariety (Raster a) = Raster $ mapStencil Edge (neighbourhoodStencil f) a
  where f nw no ne we fo ea sw so se = fromIntegral . length $ L.nub [ nw, no, ne, we, fo, ea, sw, so, se ]
{-# INLINE fvariety #-}

-- | Focal Majority - the most frequently appearing value in each neighbourhood.
fmajority :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmajority (Raster a) = Raster $ mapStencil Continue (neighbourhoodStencil f) a
  where f nw no ne we fo ea sw so se = majo [ nw, no, ne, we, fo, ea, sw, so, se ]
{-# INLINE fmajority #-}

-- | Focal Minority - the least frequently appearing value in each neighbourhood.
fminority :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fminority (Raster a) = Raster $ mapStencil Continue (neighbourhoodStencil f) a
  where f nw no ne we fo ea sw so se = mino [ nw, no, ne, we, fo, ea, sw, so, se ]
{-# INLINE fminority #-}

-- | Focal Percentage - the percentage of neighbourhood values that are equal to
-- the neighbourhood focus. Not to be confused with `fpercentile`.
fpercentage :: (Eq a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Double
fpercentage (Raster a) = Raster $ mapStencil Continue (neighbourhoodStencil f) a
  where f nw no ne we fo ea sw so se = ( bool 0 1 (nw == fo)
                                       + bool 0 1 (no == fo)
                                       + bool 0 1 (ne == fo)
                                       + bool 0 1 (we == fo)
                                       + bool 0 1 (ea == fo)
                                       + bool 0 1 (sw == fo)
                                       + bool 0 1 (so == fo)
                                       + bool 0 1 (se == fo) ) / 8
{-# INLINE fpercentage #-}

-- | Focal Percentile - the percentage of neighbourhood values that are /less/
-- than the neighbourhood focus. Not to be confused with `fpercentage`.
fpercentile :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Double
fpercentile (Raster a) = Raster $ mapStencil Continue (neighbourhoodStencil f) a
  where f nw no ne we fo ea sw so se = ( bool 0 1 (nw < fo)
                                       + bool 0 1 (no < fo)
                                       + bool 0 1 (ne < fo)
                                       + bool 0 1 (we < fo)
                                       + bool 0 1 (ea < fo)
                                       + bool 0 1 (sw < fo)
                                       + bool 0 1 (so < fo)
                                       + bool 0 1 (se < fo) ) / 8
{-# INLINE fpercentile #-}

-- `Fill def` has the highest chance of the edge pixel and the off-the-edge pixel
-- having a different value. This is until the following is addressed:
-- https://github.com/fosskers/mapalgebra/pull/3#issuecomment-379943208
-- | Focal Linkage - a description of how each neighbourhood focus is connected
-- to its neighbours. Foci of equal value to none of their neighbours will have
-- a value of 0.
flinkage :: (Default a, Eq a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Line
flinkage (Raster a) = Raster $ mapStencil (Fill def) (neighbourhoodStencil linkage) a
{-# INLINE flinkage #-}

-- | A description of lineal directions with the same encoding as `Drain`.
-- See `flinkage` and `flength`.
newtype Line = Line { _line :: Word8 }
  deriving stock   (Eq, Ord, Show)
  deriving newtype (Storable, Prim, Default)

instance NFData Line where
  rnf (Line a) = deepseq a ()

linkage :: Eq a => a -> a -> a -> a -> a -> a -> a -> a -> a -> Line
linkage nw no ne we fo ea sw so se = Line $ axes + diags
  where axes  = bool 0 2 (no == fo) + bool 0 8 (we == fo) + bool 0 16 (ea == fo) + bool 0 64 (so == fo)
        diags = bool 0 1     (nw == fo && not (testBit axes 1 || testBit axes 3))
                + bool 0 4   (ne == fo && not (testBit axes 1 || testBit axes 4))
                + bool 0 32  (sw == fo && not (testBit axes 3 || testBit axes 6))
                + bool 0 128 (se == fo && not (testBit axes 4 || testBit axes 6))
{-# INLINE linkage #-}

-- | Directions that neighbourhood foci can be connected by.
data Direction = East | NorthEast | North | NorthWest | West | SouthWest | South | SouthEast
  deriving (Eq, Ord, Enum, Show)

-- | Focal Length - the length of the lineal structure at every location. The
-- result is in "pixel units", where 1 is the height/width of one pixel.
flength :: Functor (Raster u p r c) => Raster u p r c Line -> Raster u p r c Double
flength = fmap f
  where half = 1 / 2
        root = 1 / sqrt 2
        f (Line a) = bool 0 half (testBit a 1)
                     + bool 0 half (testBit a 3)
                     + bool 0 half (testBit a 4)
                     + bool 0 half (testBit a 6)
                     + bool 0 root (testBit a 0)
                     + bool 0 root (testBit a 2)
                     + bool 0 root (testBit a 5)
                     + bool 0 root (testBit a 7)
{-# INLINE flength #-}

-- | A layout of the areal conditions of a single `Raster` pixel. It describes
-- whether each pixel corner is occupied by the same "areal zone" as the pixel
-- centre.
data Corners = Corners { _topLeft     :: !Surround
                       , _bottomLeft  :: !Surround
                       , _bottomRight :: !Surround
                       , _topRight    :: !Surround } deriving (Eq, Show)

instance NFData Corners where
  rnf (Corners tl bl br tr) = tl `deepseq` bl `deepseq` br `deepseq` tr `deepseq` ()

-- | A state of surroundedness of a pixel corner. For the examples below, the
-- bottom-left pixel is considered the focus and we're wondering about the
-- surroundedness of its top-right corner.
data Surround = Complete      -- ^ A corner has three of the same opponent against it.
                              --
                              -- The corner is considered "occupied" by the opponent value,
                              -- thus forming a diagonal areal edge.
                              --
                              -- @
                              -- [ 1 1 ]
                              -- [ 0 1 ]
                              -- @
                | OneSide     -- ^ One edge of a corner is touching an opponent, but
                              -- the other edge touches a friend.
                              --
                              -- @
                              -- [ 1 1 ]  or  [ 0 1 ]
                              -- [ 0 0 ]      [ 0 1 ]
                              -- @
                | Open        -- ^ A corner is surrounded by friends.
                              --
                              -- @
                              -- [ 0 0 ]  or  [ 0 0 ]  or  [ 1 0 ]
                              -- [ 0 0 ]      [ 0 1 ]      [ 0 0 ]
                              -- @
                | RightAngle  -- ^ Similar to `Complete`, except that the diagonal
                              -- opponent doesn't match the other two. The corner
                              -- is considered surrounded, but not "occupied".
                              --
                              -- @
                              -- [ 1 2 ]
                              -- [ 0 1 ]
                              -- @
                | OutFlow     -- ^ Similar to `Complete`, except that the area of the
                              -- focus surrounds the diagonal neighbour.
                              --
                              -- @
                              -- [ 0 1 ]
                              -- [ 0 0 ]
                              -- @
  deriving (Eq, Ord, Show)

instance NFData Surround where
  rnf s = case s of
    Complete   -> ()
    OneSide    -> ()
    Open       -> ()
    RightAngle -> ()
    OutFlow    -> ()

-- | Imagining a 2x2 neighbourhood with its focus in the bottom-left, what
-- `Surround` relationship does the focus have with the other pixels?
surround :: Eq a => a -> a -> a -> a -> Surround
surround fo tl tr br
  | up && tl == tr && tr == br = Complete
  | up && right = RightAngle
  | (up && diag) || (diag && right) = OneSide
  | diag && fo == tl && fo == br = OutFlow
  | otherwise = Open
  where up    = fo /= tl
        diag  = fo /= tr
        right = fo /= br
{-# INLINE surround #-}

-- | What is the total length of the areal edges (if there are any) at a given
-- pixel?
frontage :: Corners -> Double
frontage (Corners tl bl br tr) = f tl + f bl + f br + f tr
  where f Complete   = 1 / sqrt 2
        f OneSide    = 1 / 2
        f Open       = 0
        f RightAngle = 1
        f OutFlow    = 1 / sqrt 2

-- | Focal Partition - the areal form of each location, only considering the
-- top-right edge.
fpartition :: (Default a, Eq a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Corners
fpartition (Raster a) = Raster $ mapStencil Reflect partStencil a
{-# INLINE fpartition #-}

partStencil :: (Eq a, Default a) => Stencil Ix2 a Corners
partStencil = makeStencil (Sz2 2 2) (1 :. 0) $ \f -> do
  tl <- f (-1 :. 0)
  tr <- f (-1 :. 1)
  br <- f (0  :. 1)
  fo <- f (0  :. 0)
  pure $ Corners (surround fo tl tl fo) Open (surround fo fo br br) (surround fo tl tr br)
{-# INLINE partStencil #-}

-- | Like `fpartition`, but considers the `Surround` of all corners. Is alluded
-- to in GaCM but isn't given its own operation name.
--
-- If preparing for `ffrontage` or `farea`, you almost certainly want this
-- function and not `fpartition`.
fshape :: (Default a, Eq a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Corners
fshape (Raster a) = Raster $ mapStencil Reflect (neighbourhoodStencil f) a
  where f nw no ne we fo ea sw so se = Corners (surround fo no nw we)
                                       (surround fo so sw we)
                                       (surround fo so se ea)
                                       (surround fo no ne ea)
{-# INLINE fshape #-}

-- | Focal Frontage - the length of areal edges between each pixel and its
-- neighbourhood.
--
-- Usually, the output of `fshape` is the appropriate input for this function.
ffrontage :: Functor (Raster u p r c) => Raster u p r c Corners -> Raster u p r c Double
ffrontage = fmap frontage
{-# INLINE ffrontage #-}

-- | The area of a 1x1 square is 1. It has 8 right-triangular sections,
-- each with area 1/8.
area :: Corners -> Double
area (Corners tl bl br tr) = 1 - f tl - f bl - f br - f tr
  where f Complete = 1/8
        f OutFlow  = -1/8  -- For areas that "invade" their neighbours.
        f _        = 0
{-# INLINE area #-}

-- | Focal Area - the area of the shape made up by a neighbourhood focus and its
-- surrounding pixels. Each pixel is assumed to have length and width of 1.
--
-- Usually, the output of `fshape` is the appropriate input for this function.
farea :: Functor (Raster u p r c) => Raster u p r c Corners -> Raster u p r c Double
farea = fmap area
{-# INLINE farea #-}

-- | Focal Volume - the surficial volume under each pixel, assuming the `Raster`
-- represents elevation in some way.
fvolume :: (Fractional a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fvolume (Raster a) = Raster $ mapStencil Reflect (neighbourhoodStencil volume) a
{-# INLINE fvolume #-}

volume :: Fractional a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
volume tl up tr le fo ri bl bo br =
  ((fo * 8)  -- Simple algebra to reorganize individual volume calculations for each subtriangle.
    + nw + no
    + no + ne
    + ne + ea
    + ea + se
    + se + so
    + so + sw
    + sw + we
    + we + nw) / 24
  where nw = (tl + up + le + fo) / 4
        no = (up + fo) / 2
        ne = (up + tr + fo + ri) / 4
        we = (le + fo) / 2
        ea = (fo + ri) / 2
        sw = (le + fo + bl + bo) / 4
        so = (fo + bo) / 2
        se = (fo + ri + bo + br) / 4
{-# INLINE volume #-}

-- | Direct access to the entire neighbourhood.
neighbourhood :: Applicative f => (a -> a -> a -> a -> a -> a -> a -> a -> a -> b) -> (Ix2 -> f a) -> f b
neighbourhood g f = g <$> f (-1 :. -1) <*> f (-1 :. 0) <*> f (-1 :. 1)
                    <*> f (0  :. -1) <*> f (0  :. 0) <*> f (0  :. 1)
                    <*> f (1  :. -1) <*> f (1  :. 0) <*> f (1  :. 1)
{-# INLINE neighbourhood #-}

neighbourhoodStencil :: Default a => (a -> a -> a -> a -> a -> a -> a -> a -> a -> b) -> Stencil Ix2 a b
neighbourhoodStencil f = makeStencil (Sz2 3 3) (1 :. 1) (neighbourhood f)
{-# INLINE neighbourhoodStencil #-}

-- | Get the surficial facets for each pixel and apply some function to them.
facetStencil :: (Fractional a, Default a) => (a -> a -> a -> a -> a -> a -> a -> a -> a -> b) -> Stencil Ix2 a b
facetStencil f = makeStencil (Sz2 3 3) (1 :. 1) (neighbourhood g)
  where g nw no ne we fo ea sw so se = f ((nw + no + we + fo) / 4)
                                         ((no + fo) / 2)
                                         ((no + ne + fo + ea) / 4)
                                         ((we + fo) / 2)
                                         fo
                                         ((fo + ea) / 2)
                                         ((we + fo + sw + so) / 4)
                                         ((fo + so) / 2)
                                         ((fo + ea + so + se) / 4)
{-# INLINE facetStencil #-}

-- | The first part to the "left pseudo inverse" needed to calculate
-- a best-fitting plane of 9 points.
leftPseudo :: LA.Matrix Double
leftPseudo = LA.inv (aT <> a) <> aT
  where aT = LA.tr' a
        a  = LA.matrix 3 [ -0.5, -0.5, 1
                         , -0.5, 0, 1
                         , -0.5, 0.5, 1
                         , 0, -0.5, 1
                         , 0, 0, 1
                         , 0, 0.5, 1
                         , 0.5, -0.5, 1
                         , 0.5, 0, 1
                         , 0.5, 0.5, 1 ]

-- TODO: newtype wrapper for `Radians`?
-- | Focal Gradient - a measurement of surficial slope for each pixel relative to
-- the horizonal cartographic plane. Results are in radians, with a flat plane
-- having a slope angle of 0 and a near-vertical plane approaching \(\tau / 4\).
fgradient :: (Manifest u Ix2 Double) => Raster u p r c Double -> Raster DW p r c Double
fgradient (Raster a) = Raster $ mapStencil Reflect (facetStencil gradient) a
{-# INLINE fgradient #-}

-- | \(\tau\). One full rotation of the unit circle.
tau :: Double
tau = 6.283185307179586

-- | Given a list of \(z\) values, find the slope of the best-fit plane that
-- matches those points.
--
-- See: https://stackoverflow.com/a/16669463/643684
gradient :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
gradient nw no ne we fo ea sw so se = (tau / 2) - acos (normal vs LA.! 2)
  where vs = [ nw, no, ne, we, fo, ea, sw, so, se ]

-- | Given a list of \(z\) values, find a normal vector that /points down/ from
-- a best-fit plane toward the cartographic plane.
normal :: [Double] -> LA.Vector Double
normal = LA.normalize . zcoord (-1) . normal'

-- | A non-normalized, non-Z-corrected normal. Handy for `faspect`,
-- which needs to drop the Z and renormalize.
normal' :: [Double] -> LA.Vector Double
normal' vs = leftPseudo LA.#> LA.vector vs

-- | Replace the Z-coordinate of a Vector.
zcoord :: Double -> LA.Vector Double -> LA.Vector Double
zcoord n v = LA.vector [ v LA.! 0, v LA.! 1, n ]

-- | Focal Aspect - the compass direction toward which the surface descends most
-- rapidly. Results are in radians, with 0 or \(\tau\) being North, \(\tau / 4\)
-- being East, and so on. For areas that are essentially flat, their aspect will
-- be `Nothing`.
faspect :: Manifest u Ix2 Double => Raster u p r c Double -> Raster DW p r c (Maybe Double)
faspect (Raster a) = Raster $ mapStencil Reflect (facetStencil f) a
  where f nw no ne we fo ea sw so se = case normal' [ nw, no, ne, we, fo, ea, sw, so, se ] of
                 n | ((n LA.! 0) =~ 0) && ((n LA.! 1) =~ 0) -> Nothing
                   | otherwise -> Just $ angle (LA.normalize $ zcoord 0 n) axis
        axis = LA.vector [1, 0, 0]
{-# INLINE faspect #-}

-- | Like `faspect`, but slightly faster. Beware of nonsense results when the
-- plane is flat.
faspect' :: Manifest u Ix2 Double => Raster u p r c Double -> Raster DW p r c Double
faspect' (Raster a) = Raster $ mapStencil Reflect (facetStencil f) a
  where f nw no ne we fo ea sw so se = angle (LA.normalize $ zcoord 0 $ normal' [ nw, no, ne, we, fo, ea, sw, so , se ]) axis
        axis = LA.vector [1, 0, 0]
{-# INLINE faspect' #-}

-- | Approximate Equality. Considers two `Double` to be equal if they are less
-- than \(/tau / 1024\) apart.
(=~) :: Double -> Double -> Bool
a =~ b = abs (a - b) < 0.0061359

-- | Given two normalized (length 1) vectors in R3, find the angle between them.
angle :: LA.Vector Double -> LA.Vector Double -> Double
angle u v = acos $ LA.dot u v

-- | The main type for `fdownstream` and `fupstream`, used to calculate Focal
-- Drainage. This scheme for encoding drainage patterns is described on page 81
-- of GaCM.
--
-- ==== __Full Explanation__
--
-- Fluid can flow in or out of a square pixel in one of 256 ways. Imagine a pit,
-- whose neighbours are all higher in elevation: liquid would flow in from all
-- eight compass directions, but no liquid would flow out. Consider then a
-- neighbourhood of random heights - fluid might flow in or out of the focus in
-- any permutation of the eight directions.
--
-- The scheme for encoding these permutations in a single `Word8` as described
-- in GaCM is this:
--
-- Flow in a particular direction is represented by a power of 2:
--
-- @
-- [  1   2   4  ]
-- [  8       16 ]
-- [ 32  64  128 ]
-- @
--
-- Direction values are summed to make the encoding. If there were drainage to
-- the North, East, and SouthEast, we'd see a sum of \(2 + 16 + 128 = 146\) to
-- uniquely represent this.
--
-- Analysing a drainage pattern from a `Drain` is just as easy: check if the bit
-- corresponding to the desired direction is flipped. The `direction` function
-- handles this.
newtype Drain = Drain { _drain :: Word8 }
  deriving stock   (Eq, Ord, Show)
  deriving newtype (Storable, Prim, Default)

instance NFData Drain where
  rnf (Drain a) = deepseq a ()

-- | Focal Drainage - downstream portion. This indicates the one or more compass
-- directions of steepest descent from each location. Appropriate as the input
-- to `fupstream`.
--
-- __Note:__ Peak-like surfaces will not flow equally in all 8 directions.
-- Consider this neighbourhood:
--
-- @
-- [ 1 1 1 ]
-- [ 1 3 1 ]
-- [ 1 1 1 ]
-- @
--
-- According to the rules in GaCM for calculating the intermediate surficial
-- "facet" points for the focus, 3, we arrive at the following facet height
-- matrix:
--
-- @
-- [ 1.5 2 1.5 ]
-- [  2  3  2  ]
-- [ 1.5 2 1.5 ]
-- @
--
-- With these numbers it's clear that the corners would yield a steeper angle,
-- so our resulting `Drain` would only contain the directions of the diagonals.
fdownstream :: Manifest u Ix2 Double => Raster u p r c Double -> Raster DW p r c Drain
fdownstream (Raster a) = Raster $ mapStencil Reflect (facetStencil downstream) a
{-# INLINE fdownstream #-}

downstream :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Drain
downstream nw no ne we fo ea sw so se = Drain . snd $ foldl' f (0, 0) angles
  where f (!curr, !s) (!a, !d) | a =~ curr = (curr, s + d)
                               | a >  curr = (a, d)
                               | otherwise = (curr, s)
        angles = [ (fo - nw, 1)
                 , (fo - no, 2)
                 , (fo - ne, 4)
                 , (fo - we, 8)
                 , (fo - ea, 16)
                 , (fo - sw, 32)
                 , (fo - so, 64)
                 , (fo - se, 128) ]

-- | Focal Drainage - upstream portion. This indicates the one of more compass
-- directions from which liquid would flow into each surface location. See also
-- `fdownstream`.
fupstream :: Manifest u Ix2 Drain => Raster u p r c Drain -> Raster DW p r c Drain
fupstream (Raster a) = Raster $ mapStencil (Fill $ Drain 0) (neighbourhoodStencil f) a
  where f nw no ne we _ ea sw so se = Drain $ bool 0 1 (testBit (_drain nw) 7)
                                      + bool 0 2   (testBit (_drain no) 6)
                                      + bool 0 4   (testBit (_drain ne) 5)
                                      + bool 0 8   (testBit (_drain we) 4)
                                      + bool 0 16  (testBit (_drain ea) 3)
                                      + bool 0 32  (testBit (_drain sw) 2)
                                      + bool 0 64  (testBit (_drain so) 1)
                                      + bool 0 128 (testBit (_drain se) 0)
{-# INLINE fupstream #-}

-- | Does a given `Drain` indicate flow in a certain `Direction`?
direction :: Direction -> Drain -> Bool
direction dir (Drain d) = case dir of
  NorthWest -> testBit d 0
  North     -> testBit d 1
  NorthEast -> testBit d 2
  West      -> testBit d 3
  East      -> testBit d 4
  SouthWest -> testBit d 5
  South     -> testBit d 6
  SouthEast -> testBit d 7

-- | All `Direction`s that a `Drain` indicates flow toward.
directions :: Drain -> S.Set Direction
directions d = S.fromList $ foldl' (\acc dir -> bool acc (dir : acc) $ direction dir d) [] dirs
  where dirs = [NorthWest, North, NorthEast, West, East, SouthWest, South, SouthEast]

-- | The opposite of `directions`.
drainage :: S.Set Direction -> Drain
drainage = Drain . S.foldl' f 0
  where f acc d = case d of
          NorthWest -> acc + 1
          North     -> acc + 2
          NorthEast -> acc + 4
          West      -> acc + 8
          East      -> acc + 16
          SouthWest -> acc + 32
          South     -> acc + 64
          SouthEast -> acc + 128

-- | A count of `Word8` values across some `Raster`.
newtype Histogram = Histogram { _histogram :: VS.Vector Word } deriving (Eq, Show)

-- | Given a `Raster` of byte data, efficiently produce a `Histogram` that
-- describes value counts across the image. To be passed to `breaks`.
histogram :: Source u Ix2 Word8 => Raster u p r c Word8 -> Histogram
histogram (Raster a) = runST $ do
  acc <- VSM.replicate 256 0
  A.mapM_ (VSM.unsafeModify acc succ . fromIntegral) a
  Histogram <$> VS.unsafeFreeze acc
{-# INLINE histogram #-}

-- | Given a `Histogram`, produce a list of "colour breaks" as needed by
-- functions like `greenRed`.
breaks :: Histogram -> [Word8]
breaks (Histogram h) = take 10 . (1 :) . P.reverse . snd . VS.ifoldl' f (binWidth, []) . VS.postscanl' (+) 0 $ VS.tail h
  where binWidth     = VS.sum (VS.tail h) `div` 11  -- Yes, 11 and not 10.
        f a@(goal, acc) i n | n > goal  = (next n goal, (fromIntegral i + 1) : acc)
                            | otherwise = a
        next n goal | (n - goal) > binWidth = goal + (binWidth * (((n - goal) `div` binWidth) + 1))
                    | otherwise = goal + binWidth
