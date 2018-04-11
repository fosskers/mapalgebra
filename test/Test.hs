{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import           Data.Int
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Massiv.Array as A
-- import Data.Massiv.Array.IO
import           Data.Word
import           Geography.MapAlgebra
-- import Graphics.ColorSpace (RGBA)
import           Prelude as P
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

---

main :: IO ()
main = do
  -- tif <- BS.readFile "/home/colin/code/haskell/mapalgebra/LC81750172014163LGN00_LOW5.TIF"
  -- defaultMain $ suite tif
  defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Raster Creation"
    [ testCase "constant (256x256)"     $ length (lazy small) @?= 65536
    , testCase "constant (2^16 x 2^16)" $ length lazybig @?= 4294967296
    , testCase "Image Reading (RGBA)"   $ do
        i <- fileRGBA
        fmap (getComp . _array . _red) i @?= Right Par
    -- , testCase "Image Reading (Gray)"   $ do
    --     i <- fileY
    --     fmap (getComp . _array) i @?= Right Par
    ]
  , testGroup "Typeclass Ops"
    [ testCase "(==)" $ assertBool "(==) doesn't work" (small == small)
    , testCase "(+)"  $ strict P (lazy one + lazy one) @?= two
    ]
  , testGroup "Folds"
    [ testCase "sum (small)" $ P.sum (lazy small) @?= 327680
    -- , testCase "sum (large)" $ P.sum lazybig @?= 21474836480
    ]
  , testGroup "Local Ops"
    [ testCase "(+)"       $ P.sum (lazy small + lazy small) @?= (327680 * 2)
    , testCase "lmin"      $ strict P (lmin one two) @?= one
    , testCase "lvariety"  $ (strict P . lvariety . fmap lazy $ one :| [two]) @?= two
    , testCase "lmajority" $ (strict P . lmajority . fmap lazy $ one :| [one, two]) @?= one
    , testCase "lminority" $ (strict P . lminority . fmap lazy $ one :| [one, two]) @?= two
    -- , testCase "(+) big"   $ strict P (lazy big + lazy big) @?= bog
    ]
  , testGroup "Focal Ops"
    [ testCase "fvariety" $ strict P (fvariety one) @?= one
    , testCase "fmax"     $ strict P (fmax one) @?= one
    , testCase "fmin"     $ strict P (fmin one) @?= one
    , testGroup "fvariety"
      [ testCase "single point" singlePoint
      , testCase "2x2 same" twoByTwoSame
      , testCase "2x2 diff" twoByTwoDiff
      , testCase "3x3" threeByThree
      ]
    , testCase "flength" flengthTest
    , testCase "fpartition" fpartitionTest
    ]
  ]

one :: Raster P p 7 7 Int
one = constant P Seq 1

two :: Raster P p 7 7 Int
two = constant P Seq 2

small :: Raster P p 256 256 Int
small = constant P Seq 5

lazybig :: Raster D p 65536 65536 Int
lazybig = constant D Par 5

-- big :: Raster P p 65536 65536 Word8
-- big = constant P Par 5

-- bog :: Raster P p 65536 65536 Word8
-- bog = constant P Par 10

-- | Should have two rows and 3 columns.
-- arr :: Array U Ix2 Int
-- arr = A.fromVector Seq (2 :. 3) $ U.fromList [0..5]

-- indices :: Raster D p 256 256 Int
-- indices = fromFunction D Seq (\(r :. c) -> (r * 10) + c)

-- zoop :: Raster D p 256 256 Int
-- zoop = fromFunction D Seq (\(r :. c) -> r * c)

fileRGBA :: IO (Either String (RGBARaster p 1753 1760 Word8))
fileRGBA = fromRGBA "/home/colin/code/haskell/mapalgebra/LC81750172014163LGN00_LOW5.TIF"

-- fileY :: IO (Either String (Raster D p 1753 1760 Word8))
-- fileY = fromGray "/home/colin/code/haskell/mapalgebra/LC81750172014163LGN00_LOW5.TIF"

-- colourIt :: Raster D p 256 256 Int -> Image D RGBA Word8
-- colourIt = _array . classify invisible (greenRed [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000])

singlePoint :: Assertion
singlePoint = actual @?= expected
  where expected :: Raster B p 1 1 (S.Set Direction)
        expected = constant B Seq mempty
        actual :: Raster B p 1 1 (S.Set Direction)
        actual = strict B . flinkage $ constant P Seq (1 :: Int)

twoByTwoSame :: Assertion
twoByTwoSame = actual @?= expected
  where expected :: Raster B p 2 2 (S.Set Direction)
        expected = fromRight . fromVector Seq . V.fromList $ P.map S.fromList [ [ East, South ]
                                                                              , [ West, South ]
                                                                              , [ North,East ]
                                                                              , [ West, North ] ]
        actual :: Raster B p 2 2 (S.Set Direction)
        actual = fromRight . fmap (strict B . flinkage) . fromVector Seq $ U.fromList ([1,1,1,1] :: [Int])

twoByTwoDiff :: Assertion
twoByTwoDiff = actual @?= expected
  where expected :: Raster B p 2 2 (S.Set Direction)
        expected = fromRight . fromVector Seq . V.fromList $ P.map S.fromList [ [ SouthEast ]
                                                                              , [ SouthWest ]
                                                                              , [ NorthEast ]
                                                                              , [ NorthWest ] ]
        actual :: Raster B p 2 2 (S.Set Direction)
        actual = fromRight . fmap (strict B . flinkage) . fromVector Seq $ U.fromList ([1,2,2,1] :: [Int])

threeByThree :: Assertion
threeByThree = actual @?= expected
  where expected :: Raster B p 3 3 (S.Set Direction)
        expected = fromRight . fromVector Seq . V.fromList $ P.map S.fromList [ [ ]
                                                                              , [ South ]
                                                                              , [ ]
                                                                              , [ East ]
                                                                              , [ North, West, South, East ]
                                                                              , [ West ]
                                                                              , [ ]
                                                                              , [ North ]
                                                                              , [ ] ]
        actual :: Raster B p 3 3 (S.Set Direction)
        actual = fromRight . fmap (strict B . flinkage) . fromVector Seq $ U.fromList ([1,2,1,2,2,2,1,2,1] :: [Int])

flengthTest :: Assertion
flengthTest = actual @?= expected
  where actual :: Raster U p 3 3 Double
        actual = fromRight . fmap (strict U . flength . strict B . flinkage) . fromVector Seq $ V.fromList ([1,2,1,2,2,2,1,2,1] :: [Int])
        expected :: Raster U p 3 3 Double
        expected = fromRight . fromVector Seq $ U.fromList [ 0, 3.5, 0, 3.5, 4, 3.5, 0, 3.5, 0 ]

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Was Left"

fpartitionTest :: Assertion
fpartitionTest = actual @?= expected
  where expected :: Raster B p 2 2 Corners
        expected = fromRight . fromVector Seq $ V.fromList [ Corners Self Self Self Self
                                                           , Corners Self Self Self Self
                                                           , Corners Self Self Self Other
                                                           , Corners Self Self Self Self ]
        actual :: Raster B p 2 2 Corners
        actual = fromRight . fmap (strict B . fpartition) . fromVector Seq $ U.fromList ([1,1,2,1] :: [Int])
