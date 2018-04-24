{-# LANGUAGE DataKinds, TypeApplications #-}

module Main ( main ) where

import           Criterion.Main
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Massiv.Array as A hiding (zipWith)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           GHC.TypeLits
import           Geography.MapAlgebra
import           Graphics.ColorSpace
import qualified Numeric.LinearAlgebra as LA
import           Prelude hiding (zipWith)

---

main :: IO ()
main = do
  img   <- fileY
  rgba  <- fileRGB
  rgbaF <- fileRGB'
  defaultMain
    [
      creation
    , io
    , colouring img
    , massivOps img
    , localOps rgba img
    , hmatrix
    , conversions img
    , focalOps img
    , compositeOps rgbaF
    ]

creation :: Benchmark
creation = bgroup "Raster Creation"
           [ bench "constant 256x256"     $ nf (_array . constantB) 5
           , bench "constant 512x512"     $ nf (_array . constantB') 5
           , bench "fromFunction 256x256" $ nf (_array . functionB) (\(r :. c) -> r * c)
           , bench "fromFunction 512x512" $ nf (_array . functionB') (\(r :. c) -> r * c)
           , bench "fromVector Unboxed Int - 256x256" $ nf (_array . vectorB) uv
           , bench "fromVector Boxed Int - 256x256"   $ nf (_array . vectorB') bv ]
  where uv = U.fromList ([1..65536] :: [Int])
        bv = V.fromList ([1..65536] :: [Int])

io :: Benchmark
io = bgroup "IO"
     [ bench "fromRGBA 512x512" $ nfIO (_array . _red <$> fileRGB)
     , bench "fromRGBA (Word8 -> Double) 512x512" $ nfIO (_array . _red <$> fileRGB')
     , bench "fromGray Multiband 512x512"  $ nfIO (_array <$> fileY)
     , bench "fromGray Singleband 512x512" $ nfIO (_array <$> gray "data/gray512.tif") ]

colouring :: Raster S p 512 512 Word8 -> Benchmark
colouring img = bgroup "Colouring"
                [ bench "classify 512x512"  $ nf (_array . strict S . classify invisible cr . lazy) img
                , bench "grayscale 512x512" $ nf (_array . strict S . grayscale . lazy) img ]
  where cr = greenRed [25, 50, 75, 100, 125, 150, 175, 200, 225, 255]

massivOps :: Raster S p 512 512 Word8 -> Benchmark
massivOps img = bgroup "Massiv Operations"
                [ bench "strict S . lazy" $ nf (_array . strict S . lazy) img
                , bench "strict U . lazy" $ nf (_array . strict U . lazy) img
                , bench "strict P . lazy" $ nf (_array . strict P . lazy) img ]

localOps :: RGBARaster p 512 512 Word8 -> Raster S p 512 512 Word8 -> Benchmark
localOps (RGBARaster r g b _) img = bgroup "Local Operations"
  [ bench "fmap (+ 17) . lazy" $ nf (_array . strict S . fmap (+ 17) . lazy) img
  , bench "zipWith (+)" $ nf (_array . strict S . zipWith (+) r) g
  , bench "zipWith (/)" $ nf (_array . strict S . zipWith (/) doubles) doubles
  , bench "(+)"         $ nf (_array . strict S . (+ lazy r)) (lazy g)
  , bench "(/)"         $ nf (_array . strict S . (/ lazy doubles)) (lazy doubles)
  , bench "lmax"        $ nf (_array . strict S . lmax img) img
  , bench "lmin"        $ nf (_array . strict S . lmin img) img
  , bench "lmean"       $ nf (_array . strict S . lmean @Word8 @Double) rs
  , bench "lvariety"    $ nf (_array . strict S . lvariety) rs
  , bench "lmajority"   $ nf (_array . strict S . lmajority) rs
  , bench "lminority"   $ nf (_array . strict S . lminority) rs
  , bench "lvariance"   $ nf (fmap (_array . strict S) . lvariance) rs ]
  where rs = lazy r :| [lazy g, lazy b]

hmatrix :: Benchmark
hmatrix = bgroup "HMatrix"
          [ bench "linearSolveLS" $ nf (LA.linearSolveLS zing) (LA.col [8,8,8,8,8,8,8,8,8])
          , bench "manual - MxM"  $ nf (leftPseudo <>) (LA.col [8,8,8,8,8,8,8,8,8])
          , bench "manual - MxV"  $ nf (leftPseudo LA.#>) (LA.vector [8,8,8,8,8,8,8,8,8]) ]

conversions :: Raster S p 512 512 Word8 -> Benchmark
conversions img = bgroup "Numeric Conversion"
                  [ bench "Double -> Double via id"         $ nf id tau
                  , bench "Double -> Double via realToFrac" $ nf (realToFrac @Double @Double) tau
                  , bench "Word -> Double via realToFrac"   $ nf (realToFrac @Word8 @Double) 5
                  , bench "realToFrac on Raster"            $ nf (_array . strict S . fmap (realToFrac @Word8 @Double) . lazy) img ]

focalOps :: Raster S p 512 512 Word8 -> Benchmark
focalOps img = bgroup "Focal Operations"
               [ bench "fsum"        $ nf (_array . strict S . fsum) img
               , bench "fmean"       $ nf (_array . strict S . fmean @Word8 @Double) img
               , bench "fmax"        $ nf (_array . strict S . fmax) img
               , bench "fmin"        $ nf (_array . strict S . fmin) img
               , bench "fmajority"   $ nf (_array . strict S . fmajority) img
               , bench "fminority"   $ nf (_array . strict S . fminority) img
               , bench "fvariety"    $ nf (_array . strict S . fvariety) img
               , bench "fpercentage" $ nf (_array . strict S . fpercentage) img
               , bench "fpercentile" $ nf (_array . strict S . fpercentile) img
               , bench "flinkage"    $ nf (_array . strict B . flinkage) img
               , bench "flength"     $ nf (_array . strict S . flength . strict B . flinkage) img
               , bench "fpartition"  $ nf (_array . strict B . fpartition) img
               , bench "fshape"      $ nf (_array . strict B . fshape) img
               , bench "ffrontage"   $ nf (_array . strict S . ffrontage . strict B . fshape) img
               , bench "farea"       $ nf (_array . strict S . farea . strict B . fshape) img
               , bench "fvolume"     $ nf (_array . strict S . fvolume @Word8 @Double) img
               , bench "fgradient"   $ nf (_array . strict S . fgradient) img
               , bgroup "faspect"
                 [ bench "Unsafe" $ nf (_array . strict S . faspect') img
                 , bench "Safe"   $ nf (_array . strict B . faspect) img
                 ]
               , bgroup "fdownstream"
                 [ bench "Word" $ nf (_array . strict S . fdownstream) img
                 ]
               , bgroup "fupstream"
                 [ bench "Word" $ nf (_array . strict S . fupstream . strict S . fdownstream) img
                 ]
               ]

compositeOps :: RGBARaster p 512 512 Double -> Benchmark
compositeOps i@(RGBARaster r g _ _) = bgroup "Composite Operations"
                                      [ bench "NDVI" $ nf (_array . strict S . ndvi (lazy g)) (lazy r)
                                      , bench "EVI"  $ nf (_array . strict S . evi) i
                                      , bench "EVI + Colour" $ nf (_array . strict S . classify invisible cr . evi) i
                                      , bench "EVI + Colour + PNG (D)" $ nf (png . classify invisible cr . evi) i
                                      , bench "EVI + Colour + PNG (S)" $ nf (png . strict S . classify invisible cr . evi) i
                                      ]
  where cr = greenRed $ fmap (10 ^) [1..10]

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Was Left"

constantB :: Int -> Raster S p 256 256 Int
constantB = constant S Par

constantB' :: Int -> Raster S p 512 512 Int
constantB' = constant S Par

functionB :: (Ix2 -> Int) -> Raster S p 256 256 Int
functionB = fromFunction S Par

functionB' :: (Ix2 -> Int) -> Raster S p 512 512 Int
functionB' = fromFunction S Par

vectorB :: U.Vector Int -> Raster U p 256 256 Int
vectorB = fromRight . fromVector Par

vectorB' :: V.Vector Int -> Raster B p 256 256 Int
vectorB' = fromRight . fromVector Par

doubles :: Raster U p 512 512 Double
doubles = fromRight . fromVector Par $ U.fromList ([1..262144] :: [Double])

zing :: LA.Matrix Double
zing = LA.matrix 3 [ -0.5, -0.5, 1
                   , -0.5, 0, 1
                   , -0.5, 0.5, 1
                   , 0, -0.5, 1
                   , 0, 0, 1
                   , 0, 0.5, 1
                   , 0.5, -0.5, 1
                   , 0.5, 0, 1
                   , 0.5, 0.5, 1 ]

gray :: FilePath -> IO (Raster S p 512 512 Word8)
gray fp = do
  i <- fromGray fp
  case i of
    Left err  -> error err
    Right img -> pure img

-- fileY :: IO (Raster S p 1753 1760 Word8)
fileY :: IO (Raster S p 512 512 Word8)
fileY = do
  i <- fromGray "data/512x512.tif"
  case i of
    Left err  -> error err
    Right img -> pure img

fileRGB :: IO (RGBARaster p 512 512 Word8)
fileRGB = do
  i <- fromRGBA "data/512x512.tif"
  case i of
    Left err  -> error err
    Right img -> pure img

fileRGB' :: IO (RGBARaster p 512 512 Double)
fileRGB' = do
  i <- fromRGBA "data/512x512.tif"
  case i of
    Left err  -> error err
    Right img -> pure img

-- | See: https://en.wikipedia.org/wiki/Normalized_difference_vegetation_index
ndvi :: (KnownNat r, KnownNat c) => Raster D p r c Double -> Raster D p r c Double -> Raster D p r c Double
ndvi nir red = (nir - red) / (nir + red)
{-# INLINE ndvi #-}

-- | See: https://en.wikipedia.org/wiki/Enhanced_vegetation_index
evi :: (Fractional a, Storable a, KnownNat r, KnownNat c) => RGBARaster p r c a -> Raster D p r c a
evi (RGBARaster r g b _) = 2.5 * (numer / denom)
  where nir   = lazy g  -- fudging it.
        numer = nir - lazy r
        denom = nir + (6 * lazy r) - (7.5 * lazy b) + 1
{-# INLINE evi #-}
