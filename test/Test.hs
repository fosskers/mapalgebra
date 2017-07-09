{-# LANGUAGE DataKinds #-}

module Main where

-- import qualified Data.Array.Repa as R
-- import           Data.Functor.Identity
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
  ]

small :: Raster WebMercator 256 256 Int
small = constant 5

big :: Raster WebMercator 65536 65536 Int
big = constant 5
