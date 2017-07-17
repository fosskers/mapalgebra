{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as U
import           Geography.MapAlgebra
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Raster Creation"
    [ testCase "constant (256x256)" $ length small @?= 65536
    , testCase "constant (2^16 x 2^16)" $ length (constant 5 :: Raster p 65536 65536 Int) @?= 4294967296
    ]
  , testGroup "Folds"
    [ testCase "sum (small)" $ sum small @?= 327680
    -- takes ~4 seconds
--    , testCase "sum (large)" $ runIdentity (R.sumAllP $ _array big) @?= 21474836480
    ]
  , testGroup "Local Ops"
    [ testCase "(+)" $ sum (small + small) @?= (327680 * 2)
    -- takes ~68 seconds
--    , testCase "(+) big" $ runIdentity (R.sumAllP . _array $ big + big) @?= 21474836480 * 2
    ]
  , testGroup "Repa Behaviour"
    [ testCase "Row-Major Indexing" $ R.index arr (R.ix2 1 0) @?= 3
    ]
  ]

small :: Raster WebMercator 256 256 Int
small = constant 5

big :: Raster WebMercator 65536 65536 Int
big = constant 5

-- | Should have two rows and 3 columns.
arr :: R.Array R.U R.DIM2 Int
arr = R.fromUnboxed (R.ix2 2 3) $ U.fromList [0..5]
