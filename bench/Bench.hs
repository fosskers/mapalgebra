{-# LANGUAGE DataKinds #-}

module Main where

import           Criterion.Main
-- import qualified Data.Array.Repa as R
import           Geography.MapAlgebra
import Data.Word
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as U

---

main :: IO ()
main = defaultMain
  [ bgroup "Encoding"
    [
      bench "PNG 256 Constant"    $ nf (encodePng . grayscale) (constant 125 :: Raster p 256 256 Word8)
    , bench "PNG 256 fromList"    $ nf (encodePng . grayscale) small
    , bench "PNG 256 fromUnboxed" $ nf (encodePng . grayscale) smallV

    , bench "PNG 1024 Constant"    $ nf (encodePng . grayscale) (constant 125 :: Raster p 1024 1024 Word8)
    , bench "PNG 1024 fromList"    $ nf (encodePng . grayscale) big
    , bench "PNG 1024 fromUnboxed" $ nf (encodePng . grayscale) bigV

    , bench "TIFF 256 Constant"    $ nf (encodeTiff . grayscale) (constant 125 :: Raster p 256 256 Word8)
    , bench "TIFF 256 fromList"    $ nf (encodeTiff . grayscale) small
    , bench "TIFF 256 fromUnboxed" $ nf (encodeTiff . grayscale) smallV

    , bench "TIFF 1024 Constant"    $ nf (encodeTiff . grayscale) (constant 125 :: Raster p 1024 1024 Word8)
    , bench "TIFF 1024 fromList"    $ nf (encodeTiff . grayscale) big
    , bench "TIFF 1024 fromUnboxed" $ nf (encodeTiff . grayscale) bigV
    ]
  ]
  where small :: Raster p 256 256 Word8
        small = fromJust . fromList $ replicate (256*256) 125

        smallV :: Raster p 256 256 Word8
        smallV = fromJust . fromUnboxed $ U.replicate (256*256) 125

        big :: Raster p 1024 1024 Word8
        big = fromJust . fromList $ replicate (1024*1024) 125

        bigV :: Raster p 1024 1024 Word8
        bigV = fromJust . fromUnboxed $ U.replicate (1024*1024) 125
