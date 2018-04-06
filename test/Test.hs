{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Codec.Picture
import qualified Data.ByteString as BS
import           Data.Int
import           Data.Massiv.Array as A
import qualified Data.Massiv.Array.Manifest.Vector as A
import qualified Data.Vector.Unboxed as U
import           Geography.MapAlgebra
import           Prelude as P
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = do
  -- tif <- BS.readFile "/home/colin/code/haskell/mapalgebra/LC81750172014163LGN00_LOW5.TIF"
  -- defaultMain $ suite tif
  defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Raster Creation"
    [ -- testCase "constant (256x256)" $ length small @?= 65536
    -- , testCase "constant (2^16 x 2^16)" $ length big @?= 4294967296
    -- , testCase "fromImage (256x256)" $ fmap length (NE.head <$> fromImage img :: Maybe (Raster p 256 256 Word8)) @?= Just 65536
    ]
  , testGroup "Typeclass Ops"
    [ testCase "(==)" $ assertBool "(==) doesn't work" (small == small)
    , testCase "(+)" $ assertBool "(+) doesn't work" (strict P (lazy one + lazy one) == two)
    ]
  , testGroup "Folds"
    [ testCase "sum (small)" $ P.sum (lazy small) @?= 327680
    -- takes ~4 seconds
--    , testCase "sum (large)" $ runIdentity (R.sumAllP $ _array big) @?= 21474836480
    ]
  , testGroup "Local Ops"
    [ testCase "(+)" $ P.sum (lazy small + lazy small) @?= (327680 * 2)
    -- takes ~68 seconds
--    , testCase "(+) big" $ runIdentity (R.sumAllP . _array $ big + big) @?= 21474836480 * 2
    ]
  , testGroup "Focal Ops"
    [ testCase "fvariety" $ zing fvariety one --(computeAs P . _array $ fvariety one) @?= _array one
    , testCase "fmax" $ zing fmax one -- @?= one
    , testCase "fmin" $ zing fmin one -- @?= one
    ]
  , testGroup "Repa Behaviour"
    [ -- testCase "Row-Major Indexing" $ R.index arr (R.ix2 1 0) @?= 3
    ]
  -- , testGroup "JuicyPixels Behaviour"
    -- [ testCase "Initial Image Height" $ (imageHeight <$> i) @?= Just 1753
    -- , testCase "Initial Image Width"  $ (imageWidth  <$> i) @?= Just 1760
    -- , testCase "Repa'd Array Size"    $ (R.extent . fromRGBA <$> i) @?= Just (Z :. 1753 :. 1760 :. 4)
    -- ]
  ]

zing :: (Prim a, Eq a, Show a, Load u Ix2 a) =>
  (Raster P p r c a -> Raster u p r c a) -> Raster P p r c a -> Assertion
zing f r = (computeAs P . _array $ f r) @?= _array r

one :: Raster P p 7 7 Int
one = constant P Seq 1

two :: Raster P p 7 7 Int
two = constant P Seq 2

small :: Raster P WebMercator 256 256 Int
small = constant P Seq 5

big :: Raster P WebMercator 65536 65536 Int
big = constant P Par 5

-- | Should have two rows and 3 columns.
arr :: Array U Ix2 Int
arr = A.fromVector Seq (2 :. 3) $ U.fromList [0..5]

img :: Image Pixel8
img = generateImage (\_ _ -> 5) 256 256

getImage :: BS.ByteString -> Maybe (Image PixelRGBA8)
getImage bs = either (const Nothing) Just (decodeTiff bs) >>= f
  where f (ImageRGBA8 i) = Just i
        f _ = Nothing
