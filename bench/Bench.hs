{-# LANGUAGE DataKinds, TypeApplications #-}

module Main where

import           Criterion.Main
import           Data.Massiv.Array as A
import           Data.Monoid ((<>))
import           Data.Word (Word8)
import           Geography.MapAlgebra
import qualified Numeric.LinearAlgebra as LA

---

main :: IO ()
main = do
  img <- fileY
  defaultMain
    [ bgroup "Encoding"
      [ bgroup "Grayscale"
        [
      --   bench "encodePng - 256"  $ nf encodePng gray256
      -- , bench "encodePng - 1024" $ nf encodePng gray1024

      -- , bench "encodeTiff - 256"  $ nf encodeTiff gray256
      -- , bench "encodeTiff - 1024" $ nf encodeTiff gray1024

      -- , bench "PNG 256 Constant"    $ nf (encodePng . grayscale) (constant 125 :: Raster p 256 256 Word8)
      -- , bench "PNG 256 fromList"    $ nf (encodePng . grayscale) small
      -- , bench "PNG 256 fromUnboxed" $ nf (encodePng . grayscale) smallV

      -- , bench "PNG 1024 Constant"    $ nf (encodePng . grayscale) (constant 125 :: Raster p 1024 1024 Word8)
      -- , bench "PNG 1024 fromList"    $ nf (encodePng . grayscale) big
      -- , bench "PNG 1024 fromUnboxed" $ nf (encodePng . grayscale) bigV

      -- , bench "TIFF 256 Constant"    $ nf (encodeTiff . grayscale) (constant 125 :: Raster p 256 256 Word8)
      -- , bench "TIFF 256 fromList"    $ nf (encodeTiff . grayscale) small
      -- , bench "TIFF 256 fromUnboxed" $ nf (encodeTiff . grayscale) smallV

      -- , bench "TIFF 1024 Constant"    $ nf (encodeTiff . grayscale) (constant 125 :: Raster p 1024 1024 Word8)
      -- , bench "TIFF 1024 fromList"    $ nf (encodeTiff . grayscale) big
      -- , bench "TIFF 1024 fromUnboxed" $ nf (encodeTiff . grayscale) bigV
        ]
      , bgroup "RGBA"
        [
        -- bench "generateImage - 256"  $ nf pixelImg 256
      -- , bench "generateImage - 1024" $ nf pixelImg 1024
      -- , bench "encodePng - 256"  $ nf encodePng rgba256
      -- , bench "encodePng - 1024" $ nf encodePng rgba1024

      -- , bench "encodeTiff - 256"  $ nf encodeTiff rgba256
      -- , bench "encodeTiff - 1024" $ nf encodeTiff rgba1024

      -- , bench "rgba - 256"  $ nf (encodePng . rgba . classify invisible cmap) small
      -- , bench "rgba - 256 - 2 ops"  $ nf (\r -> encodePng . rgba . classify invisible cmap $ r + r + r) small
      -- , bench "rgba - 1024" $ nf (encodePng . rgba . classify invisible cmap) big
      -- , bench "rgba - 1024 - 2 ops"  $ nf (\r -> encodePng . rgba . classify invisible cmap $ r + r + r) big
        ]
      ]
    -- , bgroup "Massiv Operations"
    --   [ bench "strict S . lazy" $ nf (_array . strict S . lazy) img
    --   , bench "strict U . lazy" $ nf (_array . strict U . lazy) img
    --   , bench "strict P . lazy" $ nf (_array . strict P . lazy) img
    --   ]
    , bgroup "Local Operations"
      [ bench "lmax" $ nf (_array . strict S . lmax img) img
        -- bench "classify 256" $ nf (classify (PixelRGBA8 0 0 0 0) gray) small
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
      [ bench "fsum"  $ nf (_array . strict S . fsum) img
      -- , bench "fmean" $ nf (_array . strict S . fmean) img
      , bgroup "faspect"
        [ bench "Unsafe" $ nf (_array . strict S . faspect') img
        , bench "Safe"   $ nf (_array . strict B . faspect) img
        ]
      ]
    ]

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


-- fileY :: IO (Raster S p 1753 1760 Word8)
fileY :: IO (Raster S p 512 512 Word8)
fileY = do
  i <- fromGray "/home/colin/code/haskell/mapalgebra/chatta.tif"
  case i of
    Left err  -> putStrLn err *> pure (constant S Par 8)
    Right img -> pure img
