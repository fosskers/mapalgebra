{-# LANGUAGE DataKinds, TypeApplications #-}

module Main where

import           Criterion.Main
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Massiv.Array as A hiding (zipWith)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Geography.MapAlgebra
import qualified Numeric.LinearAlgebra as LA
import           Prelude hiding (zipWith)

---

main :: IO ()
main = do
  img <- fileY
  RGBARaster r' g' b' _ <- fileRGB
  let uv = U.fromList ([1..65536] :: [Int])
      bv = V.fromList ([1..65536] :: [Int])
      cr = greenRed [25, 50, 75, 100, 125, 150, 175, 200, 225, 255]
      rs = r' :| [g', b']
  defaultMain
    [ bgroup "Raster Creation"
      [ bench "constant 256x256"     $ nf (_array . constantB) 5
      , bench "constant 512x512"     $ nf (_array . constantB') 5
      , bench "fromFunction 256x256" $ nf (_array . functionB) (\(r :. c) -> r * c)
      , bench "fromFunction 512x512" $ nf (_array . functionB') (\(r :. c) -> r * c)
      , bench "fromVector Unboxed Int - 256x256" $ nf (_array . vectorB) uv
      , bench "fromVector Boxed Int - 256x256"   $ nf (_array . vectorB') bv
      ]
    , bgroup "IO"
      [ bench "fromRGBA 512x512" $ nfIO (_array <$> rgbaB "data/512x512.tif")
      , bench "fromGray Multiband 512x512"  $ nfIO (_array <$> fileY)
      , bench "fromGray Singleband 512x512" $ nfIO (_array <$> gray "data/gray512.tif")
      ]
    , bgroup "Colouring"
      [ bench "classify 512x512"  $ nf (_array . strict S . classify invisible cr . lazy) img
      , bench "grayscale 512x512" $ nf (_array . strict S . grayscale . lazy) img
      ]
    -- , bgroup "Massiv Operations"
    --   [ bench "strict S . lazy" $ nf (_array . strict S . lazy) img
    --   , bench "strict U . lazy" $ nf (_array . strict U . lazy) img
    --   , bench "strict P . lazy" $ nf (_array . strict P . lazy) img
    --   ]
    , bgroup "Local Operations"
      [ bench "zipWith"   $ nf (_array . strict S . zipWith (+) img) img
      , bench "lmax"      $ nf (_array . strict S . lmax img) img
      , bench "lmin"      $ nf (_array . strict S . lmin img) img
      , bench "lmean"     $ nf (_array . strict S . lmean @Word8 @Double) rs
      , bench "lvariety"  $ nf (_array . strict S . lvariety) rs
      , bench "lmajority" $ nf (_array . strict S . lmajority) rs
      , bench "lminority" $ nf (_array . strict S . lminority) rs
      , bench "lvariance" $ nf (fmap (_array . strict S) . lvariance) rs
      ]
    -- , bgroup "HMatrix"
    --   [ bench "linearSolveLS" $ nf (LA.linearSolveLS zing) (LA.col [8,8,8,8,8,8,8,8,8])
    --   , bench "manual - MxM"  $ nf (leftPseudo <>) (LA.col [8,8,8,8,8,8,8,8,8])
    --   , bench "manual - MxV"  $ nf (leftPseudo LA.#>) (LA.vector [8,8,8,8,8,8,8,8,8])
    --   ]
    -- , bgroup "Numeric Conversion"
    --   [ bench "Double -> Double via id"         $ nf id tau
    --   , bench "Double -> Double via realToFrac" $ nf (realToFrac @Double @Double) tau
    --   , bench "Word -> Double via realToFrac"   $ nf (realToFrac @Word8 @Double) 5
    --   ]
    , bgroup "Focal Operations"
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
    ]

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

rgbaB :: FilePath -> IO (Raster S p 512 512 Word8)
rgbaB = fmap (strict S . _red . fromRight) . fromRGBA

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

-- small :: Raster p 256 256 Int
-- small = fromFunction (*)

-- smallV :: Raster p 256 256 Word8
-- smallV = fromJust . fromUnboxed $ U.replicate (256*256) 1

-- big :: Raster p 1024 1024 Int
-- big = fromFunction (*)

-- bigV :: Raster p 1024 1024 Word8
-- bigV = fromJust . fromUnboxed $ U.replicate (1024*1024) 1

-- cmap :: M.Map Int PixelRGBA8
-- cmap = greenRed [1, 10, 100, 1000, 10000, 20000, 30000, 40000, 50000, 60000]

-- pixels256 :: Raster p 256 256 PixelRGBA8
-- pixels256 = constant $ PixelRGBA8 125 125 125 maxBound
-- pixels256 = classify (PixelRGBA8 0 0 0 0) gray small

-- pixels1024 :: Raster p 1024 1024 PixelRGBA8
-- pixels1024 = constant $ PixelRGBA8 125 125 125 maxBound
-- pixels1024 = classify (PixelRGBA8 0 0 0 0) gray big

-- gray256 :: Image Word8
-- gray256 = generateImage (\_ _ -> 125) 256 256

-- gray1024 :: Image Word8
-- gray1024 = generateImage (\_ _ -> 125) 1024 1024

-- pixelImg :: Int -> Image PixelRGBA8
-- pixelImg n = generateImage (\_ _ -> PixelRGBA8 125 125 125 maxBound) n n

-- rgba256 :: Image PixelRGBA8
-- rgba256 = pixelImg 256

-- rgba1024 :: Image PixelRGBA8
-- rgba1024 = pixelImg 1024

gray :: FilePath -> IO (Raster S p 512 512 Word8)
gray fp = do
  i <- fromGray fp
  case i of
    Left err  -> putStrLn err *> pure (constant S Par 8)
    Right img -> pure img

-- fileY :: IO (Raster S p 1753 1760 Word8)
fileY :: IO (Raster S p 512 512 Word8)
fileY = do
  i <- fromGray "data/512x512.tif"
  case i of
    Left err  -> putStrLn err *> pure (constant S Par 8)
    Right img -> pure img

fileRGB :: IO (RGBARaster p 512 512 Word8)
fileRGB = do
  i <- fromRGBA "data/512x512.tif"
  case i of
    Left err  -> error err
    Right img -> pure img
