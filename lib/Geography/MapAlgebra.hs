{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

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
-- * /Focal Operations/ -> Called /convolution/ elsewhere; 'massiv' has support for this (described below)
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
  -- , Traversal'
  -- , _Word8
  -- *** Creation
  , constant, fromFunction, fromVector, tiff
  -- *** Colouring
  -- | These functions and `M.Map`s can help transform a `Raster` into a state which can be further
  -- transformed into an `Image` by `rgba`.
  --
  --   * O(n): The functions can be used with `fmap` when you expect every input value to map to a unique colour.
  --   * O(nlogn): The `M.Map`s can be used with `classify` to transform /ranges/ of values into certain colours.
  --   Each Map-generating function (like `greenRed`) creates a "colour ramp" of 10 colours. So, it expects
  --   to be given a list of 10 "break points" which become the Map's keys. Any less than 10 will result
  --   in the later colours not being used. Any more than 10 will be ignored. The list of break points is
  --   assumed to be sorted.
  --   `invisible` can be used as the default value to `classify`, to make invisible any value that falls outside
  --   the range of the Maps.
  , invisible
  , gray, red, green, blue
  , greenRed, spectrum, blueGreen, purpleYellow, brownBlue
  , grayBrown, greenPurple, brownYellow, purpleGreen, purpleRed
  -- *** Image Conversion and IO
  -- | Some of these functions are re-exports from JuicyPixels. Exposing them here saves you an
  -- explicit dependency and import.
  --
  -- @
  -- rast :: Raster p 256 256 Word8
  -- rast = fromJust . fromList . concat $ replicate 256 [0..255]
  --
  -- writePng "foo.png" $ grayscale rast
  -- @
  -- , grayscale, rgba
  , encodePng, encodeTiff
  , writePng, writeTiff
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
  , fsum, fmean
  , fmax, fmin
  , fmajority, fminority, fvariety
  , fpercentage, fpercentile
  ) where

import           Codec.Picture
import           Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Default (Default)
import           Data.Foldable
import           Data.Int
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as M
import qualified Data.Massiv.Array as A
import           Data.Massiv.Array hiding (zipWith)
import qualified Data.Massiv.Array.Manifest.Vector as A
import           Data.Proxy (Proxy(..))
import           Data.Semigroup
import           Data.Typeable (Typeable)
import qualified Data.Vector.Generic as GV
import           Data.Word
import           GHC.TypeLits
import qualified Prelude as P
import           Prelude hiding (div, min, max, zipWith)

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

{-
instance Show a => Show (Raster p r c a) where
  show (Raster a) = ('\n' :) . unlines . map unwords $ groupsOf cols padded
    where (Z :. r :. c) = R.extent a
          rows = P.min r 10
          cols = P.min c 10
          window = R.extract (R.ix2 0 0) (R.ix2 rows cols) a
          list = map show $ R.toList window
          longest = maximum $ map length list
          padded = map (padTo longest) list
-}

-- | Pad whitespace to the front of a String so that it has a given length.
-- padTo :: Int -> String -> String
-- padTo n s = ((n - length s) `stimes` " ") ++ s

-- | I wish this were in the Prelude.
-- groupsOf :: Int -> [a] -> [[a]]
-- groupsOf _ [] = []
-- groupsOf n as = g : groupsOf n rest
--   where (g,rest) = splitAt n as

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

-- TODO: Semigroup
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
{-

instance (Fractional a, KnownNat r, KnownNat c) => Fractional (Raster p r c a) where
  a / b = zipWith (/) a b
  {-# INLINE (/) #-}

  fromRational = constant . fromRational

-- | Be careful - these operations will evaluate your lazy Raster. (Except
-- `length`, which has a specialized O(1) implementation.)
instance Foldable (Raster p r c) where
  foldMap f (Raster a) = R.foldAllS mappend mempty $ R.map f a
  sum (Raster a) = R.sumAllS a
  -- | O(1).
  length (Raster a) = R.size $ R.extent a
-}

-- TODO: more explicit implementations?
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

-- | \(\mathcal{O}(1)\). Force a `Raster`s representation to `D`, allowing it
-- to undergo various operations.
lazy :: Source u Ix2 a => Raster u p r c a -> Raster D p r c a
lazy (Raster a) = Raster $ delay a
{-# INLINE lazy #-}

-- | Evaluate some delayed (`D`, `DW`, or `DI`) `Raster` to some explicit `Manifest` type.
strict :: (Load v Ix2 a, Mutable u Ix2 a) => u -> Raster v p r c a -> Raster u p r c a
strict u (Raster a) = Raster $ computeAs u a
{-# INLINE strict #-}

-- | Create a `Raster` of any size which has the same value everywhere.
constant :: (KnownNat r, KnownNat c, Construct u Ix2 a) => u -> Comp -> a -> Raster u p r c a
constant u c a = fromFunction u c (const a)

-- | O(1). Create a `Raster` from a function of its row and column number respectively.
fromFunction :: forall u p r c a. (KnownNat r, KnownNat c, Construct u Ix2 a) =>
  u -> Comp -> (Ix2 -> a) -> Raster u p r c a
fromFunction u c f = Raster $ makeArrayR u c sh f
  where sh = fromInteger (natVal (Proxy :: Proxy r)) :. fromInteger (natVal (Proxy :: Proxy c))

-- | O(1). Create a `Raster` from the values of any `GV.Vector` type.
-- Will fail if the size of the Vector does not match the declared size of the `Raster`.
fromVector :: forall v p r c a. (KnownNat r, KnownNat c, GV.Vector v a, Mutable (A.ARepr v) Ix2 a, Typeable v) =>
  Comp -> v a -> Maybe (Raster (A.ARepr v) p r c a)
fromVector comp v | (r * c) == GV.length v = Just . Raster $ A.fromVector comp (r :. c) v
                  | otherwise = Nothing
  where r = fromInteger $ natVal (Proxy :: Proxy r)
        c = fromInteger $ natVal (Proxy :: Proxy c)

-- | O(1). Create a `Raster` from a JuicyPixels image. The result, if successful,
-- will contain as many Rasters as there were colour channels in the `Image`.
-- Will fail if the size of the `Image` does not match the declared size of the `Raster`s.
--
-- Example type specifications:
--
-- @
-- Image Pixel8      -> Maybe (NonEmpty (Raster p r c Word8))
-- Image PixelRGB8   -> Maybe (NonEmpty (Raster p r c Word8))
-- Image PixelRGBA8  -> Maybe (NonEmpty (Raster p r c Word8))
-- Image Pixel16     -> Maybe (NonEmpty (Raster p r c Word16))
-- Image PixelRGB16  -> Maybe (NonEmpty (Raster p r c Word16))
-- Image PixelRGBA16 -> Maybe (NonEmpty (Raster p r c Word16))
-- Image PixelF      -> Maybe (NonEmpty (Raster p r c Float))
-- Image PixelRGBF   -> Maybe (NonEmpty (Raster p r c Float))
-- @
-- fromImage :: forall p r c a. (KnownNat r, KnownNat c, Pixel a) =>
--   Image a -> Maybe (NonEmpty (Raster p r c (PixelBaseComponent a)))
-- fromImage i = unchannel $ fromBands n i
--   where n = componentCount $ pixelAt i 0 0

-- | Simple Traversals compatible with both lens and microlens.
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

-- | O(n). A simple Traversal for decoding/encoding ByteStrings as TIFFs.
--
-- ==== __Example__
--
-- Read a TIFF from the filesystem and convert it to a `Raster`:
--
-- @
-- import qualified Data.ByteString as BS
-- import qualified Data.List.NonEmpty as NE
-- import           Lens.Micro  -- from `microlens`
--
-- -- Fails if the TIFF didn't decode, or if it contained a pixel type
-- -- that you weren't expecting. If successful, it grabs the first
-- -- band it can find.
-- raster :: BS.ByteString -> Maybe (Raster p 256 256 Word8)
-- raster bytes = bytes ^? tiff . _Word8 . to NE.head
--
-- -- How many pixels does my Raster have?
-- main :: IO ()
-- main = do
--   mr <- raster \<$\> BS.readFile "\/path\/to\/tiff.tif"
--   case mr of
--     Nothing -> putStrLn "Darn."
--     Just r  -> putStrLn $ "Raster has " ++ show (length r) ++ " values."
-- @
tiff :: Traversal' BS.ByteString DynamicImage
tiff f bs = either (const $ pure bs) (\dn -> maybe bs id . dynamic <$> f dn) $ decodeTiff bs
  where dynamic (ImageY8 i)     = Just . BL.toStrict $ encodeTiff i
        dynamic (ImageY16 i)    = Just . BL.toStrict $ encodeTiff i
        dynamic (ImageRGB8 i)   = Just . BL.toStrict $ encodeTiff i
        dynamic (ImageRGB16 i)  = Just . BL.toStrict $ encodeTiff i
        dynamic (ImageRGBA8 i)  = Just . BL.toStrict $ encodeTiff i
        dynamic (ImageRGBA16 i) = Just . BL.toStrict $ encodeTiff i
        dynamic _               = Nothing
{-# INLINE tiff #-}

-- The underlying `Image` type may change. If the user does:
--     dn & byte .~ rs
-- where @rs@ is some `NonEmpty` of arbitrary length, then the `DynamicImage`
-- that started as a `ImageY8` couldn't possible end as one. Does this violate
-- any Traversal laws?

-- | O(1) to get. O(n) to set, where @n@ is the size of each Raster.
-- Getting will fail if the size of the decoded TIFF does not match the declared size of the `Raster`.
-- _Word8 :: (KnownNat r, KnownNat c) => Traversal' DynamicImage (NonEmpty (Raster p r c Word8))
-- _Word8 f di@(ImageY8    i) = maybe (pure di) (\rs -> ImageY8 . grayscale . NE.head <$> f rs) $ fromImage i
-- _Word8 f di@(ImageRGB8  i) = maybe (pure di) (\rs -> ImageRGB8 . convertRGB8 . ImageRGBA8 . rgba . crushBands <$> f rs) $ fromImage i
-- _Word8 f di@(ImageRGBA8 i) = maybe (pure di) (\rs -> ImageRGBA8 . rgba . crushBands <$> f rs) $ fromImage i
-- _Word8 _ i                 = pure i
-- {-# INLINE _Word8 #-}

-- | Separate an `R.Array` that contains a colour channel per Z-axis index
-- into a list of `Raster`s of each channel.
{-
unchannel :: forall p r c a. (S.Storable a, KnownNat r, KnownNat c) =>
  R.Array R.F R.DIM3 a -> Maybe (NonEmpty (Raster p r c a))
unchannel a | ar /= rr || ac /= rc = Nothing
            | otherwise = Just . NE.fromList $ map rast [0 .. (chans - 1)]
  where rast n = Raster $ R.traverse a (\(Z :. r :. c :. _) -> R.ix2 r c) (\f (Z :. r :. c) -> f $ R.ix3 r c n)
        (Z :. ar :. ac :. chans) = R.extent a
        rr = fromInteger $ natVal (Proxy :: Proxy r)
        rc = fromInteger $ natVal (Proxy :: Proxy c)
-}

-- | O(1). Basic conversion from JuicyPixels `Image` to a repa `R.Array`.
-- Can convert any pixel type!
-- fromBands :: Pixel p => Int -> Image p -> R.Array R.F R.DIM3 (PixelBaseComponent p)
-- fromBands n i = R.fromForeignPtr (R.ix3 (imageHeight i) (imageWidth i) n) . fst . S.unsafeToForeignPtr0 $ imageData i

-- | An invisible pixel (alpha channel set to 0).
invisible :: PixelRGBA8
invisible = PixelRGBA8 0 0 0 0

-- | Construct a colour ramp.
ramp :: Ord k => [(Word8, Word8, Word8)] -> [k] -> M.Map k PixelRGBA8
ramp colours breaks = M.fromList . P.zip breaks $ P.map (\(r,g,b) -> PixelRGBA8 r g b maxBound) colours

-- | From page 32 of /Cartographer's Toolkit/.
greenRed :: Ord k => [k] -> M.Map k PixelRGBA8
greenRed = ramp colours
  where colours = [ (0, 48, 0), (31, 79, 20), (100, 135, 68), (148, 193, 28), (193, 242, 3)
                  , (241, 255, 159), (249, 228, 227), (202, 145, 150), (153, 101, 97), (142, 38 ,18) ]

-- | From page 33 of /Cartographer's Toolkit/.
spectrum :: Ord k => [k] -> M.Map k PixelRGBA8
spectrum = ramp colours
  where colours = [ (0, 22, 51), (51, 18, 135), (150, 0, 204), (242, 13, 177), (255, 61, 61)
                  , (240, 152, 56), (248, 230, 99), (166, 249, 159), (184, 249, 212), (216, 230, 253) ]

-- | From page 34 of /Cartographer's Toolkit/.
blueGreen :: Ord k => [k] -> M.Map k PixelRGBA8
blueGreen = ramp colours
  where colours = [ (29, 43, 53), (37, 44, 95), (63, 70, 134), (89, 112, 147), (87, 124, 143)
                  , (117, 160, 125), (188, 219, 173), (239, 253, 163), (222, 214, 67), (189, 138, 55) ]

-- | From page 35 of /Cartographer's Toolkit/.
purpleYellow :: Ord k => [k] -> M.Map k PixelRGBA8
purpleYellow = ramp colours
  where colours = [ (90, 89, 78), (73, 65, 132), (107, 86, 225), (225, 67, 94), (247, 55, 55)
                  , (251, 105, 46), (248, 174, 66), (249, 219, 25), (255, 255, 0), (242, 242, 242) ]

-- | From page 36 of /Cartographer's Toolkit/.
brownBlue :: Ord k => [k] -> M.Map k PixelRGBA8
brownBlue = ramp colours
  where colours = [ (27, 36, 43), (86, 52, 42), (152, 107, 65), (182, 176, 152), (215, 206, 191)
                  , (198, 247, 0), (53, 227, 0), (30, 158, 184), (22, 109, 138), (12, 47, 122) ]

-- | From page 37 of /Cartographer's Toolkit/.
grayBrown :: Ord k => [k] -> M.Map k PixelRGBA8
grayBrown = ramp colours
  where colours = [ (64, 57, 88), (95, 96, 116), (158, 158, 166), (206, 208, 197), (215, 206, 191)
                  , (186, 164, 150), (160, 124, 98), (117, 85, 72), (90, 70, 63), (39, 21, 17) ]

-- | From page 38 of /Cartographer's Toolkit/.
greenPurple :: Ord k => [k] -> M.Map k PixelRGBA8
greenPurple = ramp colours
  where colours = [ (89, 168, 15), (158, 213, 76), (196, 237, 104), (226, 255, 158), (240, 242, 221)
                  , (248, 202, 140), (233, 161, 137), (212, 115, 132), (172, 67, 123), (140, 40, 110) ]

-- | From page 39 of /Cartographer's Toolkit/.
brownYellow :: Ord k => [k] -> M.Map k PixelRGBA8
brownYellow = ramp colours
  where colours = [ (96, 72, 96), (120, 72, 96), (168, 96, 96), (192, 120, 96), (240, 168, 72)
                  , (248, 202, 140), (254, 236, 174), (255, 244, 194), (255, 247, 219), (255, 252, 246) ]

-- | From page 40 of /Cartographer's Toolkit/.
purpleGreen :: Ord k => [k] -> M.Map k PixelRGBA8
purpleGreen = ramp colours
  where colours = [ (80, 73, 113), (117, 64, 152), (148, 116, 180), (199, 178, 214), (223, 204, 228)
                  , (218, 234, 193), (171, 214, 155), (109, 192, 103), (13, 177, 75), (57, 99, 83) ]

-- | From page 41 of /Cartographer's Toolkit/.
purpleRed :: Ord k => [k] -> M.Map k PixelRGBA8
purpleRed = ramp colours
  where colours = [ (51, 60, 255), (76, 60, 233), (99, 60, 211), (121, 60, 188), (155, 60, 155)
                  , (166, 60, 143), (188, 60, 121), (206, 60, 94), (217, 60, 83), (255, 60, 76) ]

-- | Sets each RGB channel to the key value. Example: for a `Word8` value
-- of 125, each channel will be set to 125. The alpha channel is set to 100% opacity.
gray :: Word8 -> PixelRGBA8
gray w = PixelRGBA8 w w w maxBound

-- | Every value maps to a shade of red.
red :: Word8 -> PixelRGBA8
red w = PixelRGBA8 w 0 0 maxBound

-- | Every value maps to a shade of green.
green :: Word8 -> PixelRGBA8
green w = PixelRGBA8 0 w 0 maxBound

-- | Every value maps to a shade of blue.
blue :: Word8 -> PixelRGBA8
blue w = PixelRGBA8 0 0 w maxBound

-- | O(k + 1), @k@ to evaluate the `Raster`, @1@ to convert to an `Image`.
-- This will evaluate your lazy `Raster` in parallel, becoming faster "for free"
-- the more cores you add (say, @+RTS -N4@).
-- grayscale :: Raster p r c Word8 -> Image Pixel8
-- grayscale (Raster a) = Image w h $ S.unsafeFromForeignPtr0 (R.toForeignPtr arr) (h*w)
--   where (Z :. h :. w) = R.extent arr
--         arr = runIdentity $ R.computeP a

-- | O(k + 1). Transform a `Raster` of pixels into a generic `Image`, ready
-- for further encoding into a PNG or TIFF. The same conditions as `grayscale` apply.
-- rgba :: Raster p r c PixelRGBA8 -> Image PixelRGBA8
-- rgba (Raster a) = Image w h $ S.unsafeFromForeignPtr0 (R.toForeignPtr arr) (h*w*z)
--   where (Z :. h :. w :. z) = R.extent arr
--         arr = runIdentity . R.computeP $ toRGBA a

-- | Expand a `Raster`'s inner Array into a format that JuicyPixels will like better.
-- toRGBA :: R.Array R.D R.DIM2 PixelRGBA8 -> R.Array R.D R.DIM3 Word8
-- toRGBA a = R.traverse a (\(Z :. r :. c) -> Z :. r :. c :. 4) f
--   where f g (Z :. r :. c :. 0) = (\(PixelRGBA8 w _ _ _) -> w) $ g (Z :. r :. c)
--         f g (Z :. r :. c :. 1) = (\(PixelRGBA8 _ w _ _) -> w) $ g (Z :. r :. c)
--         f g (Z :. r :. c :. 2) = (\(PixelRGBA8 _ _ w _) -> w) $ g (Z :. r :. c)
--         f g (Z :. r :. c :. _) = (\(PixelRGBA8 _ _ _ w) -> w) $ g (Z :. r :. c)

-- crushBands :: NonEmpty (Raster p r c Word8) -> Raster p r c PixelRGBA8
-- crushBands = undefined

-- TODO But the number of bands is not known ahead of time, so the `a` won't be a single,
-- known type. Yeah, the type family goes in the opposite direction. Some `Pixel a` knows
-- what its base component is, but each base component type could have many `Pixel a`
-- that use it. i.e. a `Word8` base component tells you nothing about how many channels
-- the pixel has.
--
-- So do I need a dependently-typed `NonEmpty` that knows its length? Then it could
-- form a new type family with the various Pixel types.
-- crooshBands :: Pixel a => NonEmpty (Raster p r c (PixelBaseComponent a)) -> Raster p r c a
-- crooshBands = undefined

-- | Called /LocalClassification/ in GaCM. The first argument is the value
-- to give to any index whose value is less than the lowest break in the `M.Map`.
--
-- This is a glorified `fmap` operation, but we expose it for convenience.
classify :: Ord a => b -> M.Map a b -> Raster D p r c a -> Raster D p r c b
classify def m r = fmap f r
  where f a = maybe def snd $ M.lookupLE a m

-- | Finds the minimum value at each index between two `Raster`s.
lmin :: (Ord a, Source u Ix2 a) => Raster u p r c a -> Raster u p r c a -> Raster D p r c a
lmin a b = zipWith P.min a b

-- | Finds the maximum value at each index between two `Raster`s.
lmax :: (Ord a, Source u Ix2 a) => Raster u p r c a -> Raster u p r c a -> Raster D p r c a
lmax a b = zipWith P.max a b

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

-- | Focal Addition.
fsum :: (Num a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fsum (Raster a) = Raster $ mapStencil sumStencil a

-- | Focal Mean.
fmean :: (Fractional a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmean = fmap (/9) . fsum

groupStencil :: Default a => ([a] -> b) -> Border a -> Stencil Ix2 a b
groupStencil f e = makeStencil e (3 :. 3) (1 :. 1) $ \g -> f <$> P.traverse g ixs
  where ixs = (:.) <$> [-1 .. 1] <*> [-1 .. 1]
{-# INLINE groupStencil #-}

-- | Focal Maximum.
fmax :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmax (Raster a) = Raster $ mapStencil (groupStencil P.maximum Edge) a

-- | Focal Minimum.
fmin :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmin (Raster a) = Raster $ mapStencil (groupStencil P.minimum Edge) a

-- | Focal Variety - the number of unique values in each neighbourhood.
fvariety :: (Eq a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Int
fvariety (Raster a) = Raster $ mapStencil (groupStencil (length . L.nub) Edge) a

-- | Focal Majority.
fmajority :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fmajority (Raster a) = Raster $ mapStencil (groupStencil majo Continue) a

-- | Focal Minority.
fminority :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c a
fminority (Raster a) = Raster $ mapStencil (groupStencil mino Continue) a

percStencil :: Default a => (a -> [a] -> b) -> Border a -> Stencil Ix2 a b
percStencil f e = makeStencil e (3 :. 3) (1 :. 1) $ \g ->
  f <$> g (0 :. 0) <*> sequenceA [ g (-1 :. -1), g (-1 :. 0), g (-1 :. 1)
                                 , g (0  :. -1),              g (0  :. 1)
                                 , g (1  :. -1), g (1  :. 0), g (1  :. 1) ]
{-# INLINE percStencil #-}

-- | Focal Percentage, the percentage of neighbourhood values that are equal
-- to the neighbourhood focus. Not to be confused with `fpercentile`.
fpercentage :: (Eq a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Double
fpercentage (Raster a) = Raster $ mapStencil (percStencil f Continue) a
  where f focus vs = fromIntegral (length $ filter (== focus) vs) / 8

-- | Focal Percentile, the percentage of neighbourhood values that are /less/
-- than the neighbourhood focus. Not to be confused with `fpercentage`.
fpercentile :: (Ord a, Default a, Manifest u Ix2 a) => Raster u p r c a -> Raster DW p r c Double
fpercentile (Raster a) = Raster $ mapStencil (percStencil f Continue) a
  where f focus vs = fromIntegral (length $ filter (< focus) vs) / 8
