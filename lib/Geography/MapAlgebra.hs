{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables #-}

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
  -- *** Creation
  , constant, fromFunction, fromUnboxed, fromList, fromImage, fromTiff
  -- , fromPng, fromTiff
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
  , grayscale, rgba
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
  , min, max
  -- *** Other
  -- | There is no binary form of these functions that exists without
  -- producing numerical error,  so you can't use the `foldl` family with these.
  -- Consider the average operation, where the following is /not/ true:
  -- \[
  --    \forall abc \in \mathbb{R}. \frac{\frac{a + b}{2} + c}{2} = \frac{a + b + c}{3}
  -- \]
  , mean, variety, majority, minority, variance
  -- ** Focal Operations
  -- | Operations on one `Raster`, given some polygonal neighbourhood.
  , Focal(..)
  , fsum, fmean
  , fmax, fmin
  , fmajority, fminority, fvariety
  , fpercentage, fpercentile
  ) where

import           Codec.Picture
import           Data.Array.Repa ((:.)(..), Z(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as R
import qualified Data.Array.Repa.Repr.Vector as R
import qualified Data.Array.Repa.Stencil as R
import           Data.Array.Repa.Stencil.Dim2 (makeStencil2)
import qualified Data.Array.Repa.Stencil.Dim2 as R
import           Data.Bits
import           Data.Bits.Floating
import qualified Data.ByteString as BS
import           Data.Foldable
import           Data.Functor.Identity (runIdentity)
import           Data.Int
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as M
import           Data.Proxy (Proxy(..))
import           Data.Semigroup
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import           Data.Word
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
newtype Raster p (r :: Nat) (c :: Nat) a = Raster { _array :: R.Array R.D R.DIM2 a }

-- TODO Might be able to do this sexier with Text.Printf.
-- | Renders the first 10x10 values in your Raster.
-- Be careful - this will evaluate your lazy Raster. For debugging purposes only!
instance Show a => Show (Raster p r c a) where
  show (Raster a) = ('\n' :) . unlines . map unwords $ groupsOf cols padded
    where (Z :. r :. c) = R.extent a
          rows = P.min r 10
          cols = P.min c 10
          window = R.extract (R.ix2 0 0) (R.ix2 rows cols) a
          list = map show $ R.toList window
          longest = maximum $ map length list
          padded = map (padTo longest) list

-- | Pad whitespace to the front of a String so that it has a given length.
padTo :: Int -> String -> String
padTo n s = ((n - length s) `stimes` " ") ++ s

-- | I wish this were in the Prelude.
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n as = g : groupsOf n rest
  where (g,rest) = splitAt n as

instance Functor (Raster p r c) where
  fmap f (Raster a) = Raster $ R.map f a
  {-# INLINE fmap #-}

instance (KnownNat r, KnownNat c) => Applicative (Raster p r c) where
  pure = constant
  {-# INLINE pure #-}

  -- TODO: Use strict ($)?
  fs <*> as = zipWith ($) fs as
  {-# INLINE (<*>) #-}

-- | Be careful - this will evaluate your lazy Raster.
instance Eq a => Eq (Raster p r c a) where
  (Raster a) == (Raster b) = runIdentity $ R.equalsP a b
  {-# INLINE (==) #-}

instance (Monoid a, KnownNat r, KnownNat c) => Monoid (Raster p r c a) where
  mempty = constant mempty
  {-# INLINE mempty #-}

  a `mappend` b = zipWith mappend a b
  {-# INLINE mappend #-}

instance (Num a, KnownNat r, KnownNat c) => Num (Raster p r c a) where
  a + b = zipWith (+) a b
  {-# INLINE (+) #-}

  a - b = zipWith (-) a b
  {-# INLINE (-) #-}

  a * b = zipWith (*) a b
  {-# INLINE (*) #-}

  abs = fmap abs
  signum = fmap signum
  fromInteger = constant . fromInteger

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

-- | Create a `Raster` of any size which has the same value everywhere.
constant :: (KnownNat r, KnownNat c) => a -> Raster p r c a
constant a = fromFunction (\_ _ -> a)

-- | O(1). Create a `Raster` from a function of its row and column number respectively.
fromFunction :: forall p r c a. (KnownNat r, KnownNat c) => (Int -> Int -> a) -> Raster p r c a
fromFunction f = Raster $ R.fromFunction sh (\(Z :. r :. c) -> f r c)
  where sh = R.ix2 (fromInteger $ natVal (Proxy :: Proxy r)) (fromInteger $ natVal (Proxy :: Proxy c))

-- | O(1). Create a `Raster` from the values of an unboxed `U.Vector`.
-- Will fail if the size of the Vector does not match the declared size of the `Raster`.
fromUnboxed :: forall p r c a. (KnownNat r, KnownNat c, U.Unbox a) => U.Vector a -> Maybe (Raster p r c a)
fromUnboxed v | (r * c) == U.length v = Just . Raster . R.delay $ R.fromUnboxed (R.ix2 r c) v
              | otherwise = Nothing
  where r = fromInteger $ natVal (Proxy :: Proxy r)
        c = fromInteger $ natVal (Proxy :: Proxy c)

-- | O(n). Create a `Raster` from a list of anything. Will fail if the size of the list
-- does not match the declared size of the `Raster`. In general, should be used for
-- debugging only.
fromList :: forall p r c a. (KnownNat r, KnownNat c) => [a] -> Maybe (Raster p r c a)
fromList l | (r * c) == length l = Just . Raster . R.delay $ R.fromListVector (R.ix2 r c) l
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
fromImage :: forall p r c a i. (KnownNat r, KnownNat c, Pixel a) =>
  Image a -> Maybe (NonEmpty (Raster p r c (PixelBaseComponent a)))
fromImage i = unchannel $ fromBands n i
  where n = componentCount $ pixelAt i 0 0

-- | O(n). Create up to four `Raster`s from a TIFF image, depending on how many of the RGBA
-- channels it stores as separate "bands".
-- Will fail if the size of the decoded TIFF does not match the declared size of the `Raster`,
-- or if the TIFF contains values of type other than `Word8`.
--
-- ==== __Example__
--
-- Read a TIFF from the filesystem and convert it to a `Raster`:
--
-- @
-- import qualified Data.ByteString as BS
-- import Data.List.NonEmpty ((:|))
--
-- raster :: BS.ByteString -> Raster p 256 256 Word8
-- raster bytes = case fromTiff bytes of
--   Nothing -> constant 0  -- Reading failed! Return a sad default...
--   Just (r :| _) -> r     -- Reading successful! Grab the Red channel for some reason.
--
-- -- | How many pixels does my Raster have?
-- main :: IO ()
-- main = do
--   r <- fmap raster $ BS.readFile "\/path\/to\/tiff.tif"
--   putStrLn $ "Raster has " ++ show (length r) ++ " values."
-- @
fromTiff :: forall p r c a. (KnownNat r, KnownNat c) => BS.ByteString -> Maybe (NonEmpty (Raster p r c Word8))
fromTiff bs = either (const Nothing) Just (decodeTiff bs) >>= f
  where f (ImageY8    img) = unchannel $ fromBands 1 img
        f (ImageRGB8  img) = unchannel $ fromBands 3 img
        f (ImageRGBA8 img) = unchannel $ fromBands 4 img
        f _ = Nothing

-- soup :: IO (Maybe (NonEmpty (Raster p 1753 1760 Word8)))
-- soup = fromTiff <$> BS.readFile "/home/colin/code/haskell/mapalgebra/LC81750172014163LGN00_LOW5.TIF"

{-
colourIt :: IO ()
colourIt = do
  mrs <- soup
  case mrs of
    Nothing -> putStrLn "Failed to read the tiff"
    Just rs -> writePng "/home/colin/code/haskell/mapalgebra/colour-it-up.png" png
      where png = rgba $ classify invisible (greenRed [1, 25, 50, 75, 100, 125, 150, 175, 200, 225]) avg
            avg = mean $ NE.map (fmap fromIntegral) rs
-}

-- | Separate an `R.Array` that contains a colour channel per Z-axis index
-- into a list of `Raster`s of each channel.
unchannel :: forall p r c a. (S.Storable a, KnownNat r, KnownNat c) =>
  R.Array R.F R.DIM3 a -> Maybe (NonEmpty (Raster p r c a))
unchannel a | ar /= rr || ac /= rc = Nothing
            | otherwise = Just . NE.fromList $ map rast [0 .. (chans - 1)]
  where rast n = Raster $ R.traverse a (\(Z :. r :. c :. _) -> R.ix2 r c) (\f (Z :. r :. c) -> f $ R.ix3 r c n)
        (Z :. ar :. ac :. chans) = R.extent a
        rr = fromInteger $ natVal (Proxy :: Proxy r)
        rc = fromInteger $ natVal (Proxy :: Proxy c)

-- | O(1). Basic conversion from JuicyPixels `Image` to a repa `R.Array`.
-- Can convert any pixel type!
fromBands :: Pixel p => Int -> Image p -> R.Array R.F R.DIM3 (PixelBaseComponent p)
fromBands n i = R.fromForeignPtr (R.ix3 (imageHeight i) (imageWidth i) n) . fst . S.unsafeToForeignPtr0 $ imageData i

-- | An invisible pixel (alpha channel set to 0).
invisible :: PixelRGBA8
invisible = PixelRGBA8 0 0 0 0

-- | Construct a colour ramp.
ramp :: Ord k => [(Word8, Word8, Word8)] -> [k] -> M.Map k PixelRGBA8
ramp colours breaks = M.fromList . zip breaks $ map (\(r,g,b) -> PixelRGBA8 r g b maxBound) colours

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
grayscale :: Raster p r c Word8 -> Image Pixel8
grayscale (Raster a) = Image w h $ S.unsafeFromForeignPtr0 (R.toForeignPtr arr) (h*w)
  where (Z :. h :. w) = R.extent arr
        arr = runIdentity $ R.computeP a

-- | O(k + 1). Transform a `Raster` of pixels into a generic `Image`, ready
-- for further encoding into a PNG or TIFF. The same conditions as `grayscale` apply.
rgba :: Raster p r c PixelRGBA8 -> Image PixelRGBA8
rgba (Raster a) = Image w h $ S.unsafeFromForeignPtr0 (R.toForeignPtr arr) (h*w*z)
  where (Z :. h :. w :. z) = R.extent arr
        arr = runIdentity . R.computeP $ toRGBA a

-- | Expand a `Raster`'s inner Array into a format that JuicyPixels will like better.
toRGBA :: R.Array R.D R.DIM2 PixelRGBA8 -> R.Array R.D R.DIM3 Word8
toRGBA a = R.traverse a (\(Z :. r :. c) -> Z :. r :. c :. 4) f
  where f g (Z :. r :. c :. 0) = (\(PixelRGBA8 w _ _ _) -> w) $ g (Z :. r :. c)
        f g (Z :. r :. c :. 1) = (\(PixelRGBA8 _ w _ _) -> w) $ g (Z :. r :. c)
        f g (Z :. r :. c :. 2) = (\(PixelRGBA8 _ _ w _) -> w) $ g (Z :. r :. c)
        f g (Z :. r :. c :. _) = (\(PixelRGBA8 _ _ _ w) -> w) $ g (Z :. r :. c)

-- | Called /LocalClassification/ in GaCM. The first argument is the value
-- to give to any index whose value is less than the lowest break in the `M.Map`.
--
-- This is a glorified `fmap` operation, but we expose it for convenience.
classify :: Ord a => b -> M.Map a b -> Raster p r c a -> Raster p r c b
classify def m r = fmap f r
  where f a = maybe def snd $ M.lookupLE a m

-- | Finds the minimum value at each index between two `Raster`s.
min :: Ord a => Raster p r c a -> Raster p r c a -> Raster p r c a
min (Raster a) (Raster b) = Raster $ R.zipWith P.min a b

-- | Finds the maximum value at each index between two `Raster`s.
max :: Ord a => Raster p r c a -> Raster p r c a -> Raster p r c a
max (Raster a) (Raster b) = Raster $ R.zipWith P.max a b

-- | Averages the values per-index of all `Raster`s in a collection.
mean :: (Fractional a, KnownNat r, KnownNat c) => NonEmpty (Raster p r c a) -> Raster p r c a
mean (a :| as) = (/ len) <$> foldl' (+) a as
  where len = 1 + fromIntegral (length as)

-- | The count of unique values at each shared index.
variety :: (KnownNat r, KnownNat c, Eq a) => NonEmpty (Raster p r c a) -> Raster p r c Int
variety = fmap (length . NE.nub) . sequenceA

-- | The most frequently appearing value at each shared index.
majority :: (KnownNat r, KnownNat c, Ord a) => NonEmpty (Raster p r c a) -> Raster p r c a
majority = fmap majo . sequenceA

-- | Find the most common value in some `Foldable`.
majo :: (Foldable t, Ord a) => t a -> a
majo = fst . g . f
  where f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty
        g = foldl1 (\(a,c) (k,v) -> if c < v then (k,v) else (a,c)) . M.toList
{-# INLINE majo #-}

-- | The least frequently appearing value at each shared index.
minority :: (KnownNat r, KnownNat c, Ord a) => NonEmpty (Raster p r c a) -> Raster p r c a
minority = fmap mino . sequenceA

-- | Find the least common value in some `Foldable`.
mino :: (Foldable t, Ord a) => t a -> a
mino = fst . g . f
  where f = foldl' (\m a -> M.insertWith (+) a 1 m) M.empty
        g = foldl1 (\(a,c) (k,v) -> if c > v then (k,v) else (a,c)) . M.toList
{-# INLINE mino #-}

-- | A measure of how spread out a dataset is. This calculation will fail
-- with `Nothing` if a length 1 list is given.
variance :: (KnownNat r, KnownNat c, Fractional a) => NonEmpty (Raster p r c a) -> Maybe (Raster p r c a)
variance (_ :| []) = Nothing
variance rs = Just (f <$> sequenceA rs)
  where len = fromIntegral $ length rs
        avg ns = (/ len) $ sum ns
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
zipWith :: (a -> b -> d) -> Raster p r c a -> Raster p r c b -> Raster p r c d
zipWith f (Raster a) (Raster b) = Raster $ R.zipWith f a b
{-# INLINE zipWith #-}

-- | Focal Addition.
fsum :: Num a => Raster p r c a -> Raster p r c a
fsum (Raster a) = Raster . R.delay $ R.mapStencil2 (R.BoundConst 0) neighbourhood a
  where neighbourhood = [R.stencil2| 1 1 1
                                     1 1 1
                                     1 1 1 |]

-- | Focal Mean.
fmean :: Fractional a => Raster p r c a -> Raster p r c a
fmean (Raster a) = Raster . R.delay $ R.mapStencil2 (R.BoundConst 0) neighbourhood a
  where neighbourhood = R.makeStencil (R.ix2 3 3) f
        f (Z :. -1 :. -1) = Just $ 1/9
        f (Z :. -1 :.  0) = Just $ 1/9
        f (Z :. -1 :.  1) = Just $ 1/9
        f (Z :.  0 :. -1) = Just $ 1/9
        f (Z :.  0 :.  0) = Just $ 1/9
        f (Z :.  0 :.  1) = Just $ 1/9
        f (Z :.  1 :. -1) = Just $ 1/9
        f (Z :.  1 :.  0) = Just $ 1/9
        f (Z :.  1 :.  1) = Just $ 1/9
        f _               = Nothing

-- | Focal Maximum.
fmax :: (Focal a, Ord a) => Raster p r c a -> Raster p r c a
fmax (Raster a) = Raster . R.map maximum $ focal R.BoundClamp a

-- | Focal Minimum.
fmin :: (Focal a, Ord a) => Raster p r c a -> Raster p r c a
fmin (Raster a) = Raster . R.map minimum $ focal R.BoundClamp a

-- | Focal Variety.
fvariety :: (Focal a, Eq a) => Raster p r c a -> Raster p r c Int
fvariety (Raster a) = Raster . R.map (length . L.nub) $ focal R.BoundClamp a

-- | Focal Majority.
fmajority :: (Focal a, Ord a) => Raster p r c a -> Raster p r c a
fmajority (Raster a) = Raster . R.map majo $ focal R.BoundClamp a

-- | Focal Minority.
fminority :: (Focal a, Ord a) => Raster p r c a -> Raster p r c a
fminority (Raster a) = Raster . R.map mino $ focal R.BoundClamp a

-- TODO Not sure about the `Boundary` value to use here.
-- | Focal Percentage, the percentage of neighbourhood values that are equal
-- to the neighbourhood focus. Not to be confused with `fpercentile`.
fpercentage :: (Focal a, Eq a) => Raster p r c a -> Raster p r c Double
fpercentage (Raster a) = Raster . R.map f $ focal R.BoundClamp a
  where f vs = fromIntegral (length . filter (== head vs) $ tail vs) / 8

-- | Focal Percentile, the percentage of neighbourhood values that are /less/
-- than the neighbourhood focus. Not to be confused with `fpercentage`.
fpercentile :: (Focal a, Ord a) => Raster p r c a -> Raster p r c Double
fpercentile (Raster a) = Raster . R.map f $ focal R.BoundClamp a
  where f vs = fromIntegral (length . filter (< head vs) $ tail vs) / 8

-- | Yield all the values in a neighbourhood for further scrutiny.
-- The first value of the list is the neighbourhood focus.
focal :: Focal a => R.Boundary Integer -> R.Array R.D R.DIM2 a -> R.Array R.D R.DIM2 [a]
focal b a = R.map unpack . R.mapStencil2 b focalStencil $ R.map common a
{-# INLINE focal #-}

-- | A stencil used to bit-pack each value of a focal neighbourhood into
-- a single `Integer`. Once packed, you can `fmap` over the resulting `Raster`
-- to unpack the `Integer` and scrutinize the values freely (i.e. determine
-- the maximum value, etc).
focalStencil :: R.Stencil R.DIM2 Integer
focalStencil = R.makeStencil (R.ix2 3 3) f
  where f :: R.DIM2 -> Maybe Integer
        f (Z :.  0 :.  0) = Just 1
        f (Z :. -1 :.  0) = Just (2^64)
        f (Z :. -1 :.  1) = Just (2^128)
        f (Z :.  0 :. -1) = Just (2^192)
        f (Z :. -1 :. -1) = Just (2^256)
        f (Z :.  0 :.  1) = Just (2^320)
        f (Z :.  1 :. -1) = Just (2^384)
        f (Z :.  1 :.  0) = Just (2^448)
        f (Z :.  1 :.  1) = Just (2^512)
        f _               = Nothing

-- | Any type which is meaningful to perform focal operations on.
--
-- Law:
--
-- @
-- back (common v) == v
-- @
class Focal a where
  -- | Convert a value into an `Integer` in a way that preserves its bit layout.
  common :: a -> Integer
  -- | Shave the first 64 bits off an `Integer` and recreate the original value.
  back :: Integer -> a

instance Focal Word8 where
  common = toInteger
  back = fromInteger

instance Focal Word32 where
  common = toInteger
  back = fromIntegral

instance Focal Word64 where
  common = toInteger
  back = fromIntegral  -- Keeps precisely the first 64 bits of the Integer.

instance Focal Float where
  common = common . coerceToWord
  back = coerceToFloat . back

instance Focal Double where
  common = common . coerceToWord
  back = coerceToFloat . back

instance Focal Int where
  -- Go through `Word` to avoid sign-bit trickery.
  common = toInteger . (\n -> fromIntegral n :: Word64)
  back = fromIntegral . (\n -> back n :: Word64)

instance Focal Int32 where
  common = toInteger . (\n -> fromIntegral n :: Word32)
  back = fromIntegral . (\n -> back n :: Word32)

instance Focal Int64 where
  common = toInteger . (\n -> fromIntegral n :: Word64)
  back = fromIntegral . (\n -> back n :: Word64)

-- | Unpack the 9 original values that were packed into an `Integer` during a Focal op.
unpack :: Focal a => Integer -> [a]
unpack = take 9 . L.unfoldr (\n -> Just (back n, shiftR n 64))
