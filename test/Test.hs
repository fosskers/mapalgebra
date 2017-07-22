{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.Array.Repa as R
import           Data.Int
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           Geography.MapAlgebra
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Raster Creation"
    [ testCase "constant (256x256)" $ length small @?= 65536
    , testCase "constant (2^16 x 2^16)" $ length big @?= 4294967296
    ]
  , testGroup "Typeclass Ops"
    [ testCase "(==)" $ assert (small == small)
    , testCase "(+)" $ assert (one + one == two)
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
  , testGroup "Focal Typeclass"
    [ testProperty "Word32" (\(v :: Word32) -> back (common v) == v)
    , testProperty "Word64" (\(v :: Word64) -> back (common v) == v)
    , testProperty "Float"  (\(v :: Float) -> back (common v) == v)
    , testProperty "Double" (\(v :: Double) -> back (common v) == v)
    , testProperty "Int"    (\(v :: Int) -> back (common v) == v)
    , testProperty "Int32"  (\(v :: Int32) -> back (common v) == v)
    , testProperty "Int64"  (\(v :: Int64) -> back (common v) == v)
    ]
  , testGroup "Repa Behaviour"
    [ testCase "Row-Major Indexing" $ R.index arr (R.ix2 1 0) @?= 3
    ]
  ]

one :: Raster p 10 10 Int
one = constant 1

two :: Raster p 10 10 Int
two = constant 2

small :: Raster WebMercator 256 256 Int
small = constant 5

big :: Raster WebMercator 65536 65536 Int
big = constant 5

-- | Should have two rows and 3 columns.
arr :: R.Array R.U R.DIM2 Int
arr = R.fromUnboxed (R.ix2 2 3) $ U.fromList [0..5]

focal :: Maybe (Raster p 5 5 Int)
focal = fromUnboxed $ U.fromList [1, 1, 1, 1, 1
                                 ,1, 2, 2, 2, 1
                                 ,1, 2, 3, 2, 1
                                 ,1, 2, 2, 2, 1
                                 ,1, 1, 1, 1, 1]
