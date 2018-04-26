{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications, ImplicitParams #-}

module Main ( main ) where

import           Data.Int
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Massiv.Array as A
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import           Data.Word
import           Geography.MapAlgebra
import qualified Numeric.LinearAlgebra as LA
import           Prelude as P
import           Test.HUnit.Approx
import qualified Test.QuickCheck.Arbitrary as QC
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Raster Creation"
    [ testCase "constant (256x256)"     $ length (lazy small) @?= 65536
    , testCase "constant (2^16 x 2^16)" $ length lazybig @?= 4294967296
    , testCase "Image Reading (RGBA)"   $ do
        i <- fileRGBA
        fmap (getComp . _array . _red) i @?= Right Par
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
    , testGroup "flinkage"
      [ testCase "single point" singlePoint
      , testCase "2x2 same" twoByTwoSame
      , testCase "2x2 diff" twoByTwoDiff
      , testCase "3x3" threeByThree
      ]
    , testCase "flength" flengthTest
    , testCase "fpartition" fpartitionTest
    , testCase "fshape" fshapeTest
    , testCase "ffrontage" ffrontageTest
    , testGroup "farea"
      [ testCase "3x3 Open" fareaOpen
      , testCase "3x3 Centre" fareaCentre
      , testCase "4x4 Complex" fareaComplex
      ]
    , testGroup "fvolume"
      [ testCase "3x3 Flat" fvolumeFlat
      , testCase "3x3 Hill" fvolumeHill
      ]
    , testProperty "Least Squares" leastSquares
    , testGroup "fgradient"
      [ testCase "3x3 Flat" fgradientFlat
      , testCase "3x3 (tau/8)" fgradient45
      ]
    , testGroup "faspect"
      [ testCase "3x3 Flat" faspectFlat
      , testCase "3x3 East" faspectEast
      , testCase "3x3 South" faspect45
      ]
    , testGroup "fdownstream"
      [ testCase "3x3 Spikey" fdownstream4
      , testCase "3x3 Flat" fdownstreamFlat
      , testCase "3x3 Peak" fdownstreamPeak
      , testCase "3x3 Pit"  fdownstreamPit
      ]
    , testGroup "fupstream"
      [ testCase "3x3 Peak" fupstreamPeak
      , testCase "3x3 Flat" fupstreamFlat
      ]
    ]
  ]

one :: Raster P p 7 7 Word
one = constant P Seq 1

two :: Raster P p 7 7 Word
two = constant P Seq 2

small :: Raster P p 256 256 Int
small = constant P Seq 5

lazybig :: Raster D p 65536 65536 Int
lazybig = constant D Par 5

-- big :: Raster P p 65536 65536 Word8
-- big = constant P Par 5

-- bog :: Raster P p 65536 65536 Word8
-- bog = constant P Par 10

fileRGBA :: IO (Either String (RGBARaster p 512 512 Word8))
fileRGBA = fromRGBA "data/512x512.tif"

singlePoint :: Assertion
singlePoint = actual @?= expected
  where expected :: Raster B p 1 1 Line
        expected = constant B Seq (Line 0)
        actual :: Raster B p 1 1 Line
        actual = strict B . flinkage $ constant P Seq (1 :: Int)

twoByTwoSame :: Assertion
twoByTwoSame = actual @?= expected
  where expected :: Raster S p 2 2 Line
        expected = fromRight . fromVector Seq . VS.fromList
          $ P.map (Line . _drain . drainage . S.fromList) [ [ East, South ]
                                                          , [ West, South ]
                                                          , [ North,East ]
                                                          , [ West, North ] ]
        actual :: Raster S p 2 2 Line
        actual = fromRight . fmap (strict S . flinkage) . fromVector Seq $ U.fromList ([1,1,1,1] :: [Int])

twoByTwoDiff :: Assertion
twoByTwoDiff = actual @?= expected
  where expected :: Raster S p 2 2 Line
        expected = fromRight . fromVector Seq . VS.fromList
          $ P.map (Line . _drain . drainage . S.fromList) [ [ SouthEast ]
                                                          , [ SouthWest ]
                                                          , [ NorthEast ]
                                                          , [ NorthWest ] ]
        actual :: Raster S p 2 2 Line
        actual = fromRight . fmap (strict S . flinkage) . fromVector Seq $ U.fromList ([1,2,2,1] :: [Int])

threeByThree :: Assertion
threeByThree = actual @?= expected
  where expected :: Raster S p 3 3 Line
        expected = fromRight . fromVector Seq . VS.fromList
          $ P.map (Line . _drain . drainage . S.fromList) [ [ ]
                                                          , [ South ]
                                                          , [ ]
                                                          , [ East ]
                                                          , [ North, West, South, East ]
                                                          , [ West ]
                                                          , [ ]
                                                          , [ North ]
                                                          , [ ] ]
        actual :: Raster S p 3 3 Line
        actual = fromRight . fmap (strict S . flinkage) . fromVector Seq $ U.fromList ([1,2,1,2,2,2,1,2,1] :: [Int])

flengthTest :: Assertion
flengthTest = actual @?= expected
  where actual :: Raster U p 3 3 Double
        actual = strict U . flength . flinkage . fromRight . fromVector Seq $ VS.fromList ([1,2,1,2,2,2,1,2,1] :: [Int])
        expected :: Raster U p 3 3 Double
        expected = fromRight . fromVector Seq $ U.fromList [ 0, 0.5, 0, 0.5, 2, 0.5, 0, 0.5, 0 ]

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Was Left"

fpartitionTest :: Assertion
fpartitionTest = actual @?= expected
  where expected :: Raster B p 2 2 Corners
        expected = fromRight . fromVector Seq $ V.fromList [ Corners Open Open Open Open
                                                           , Corners Open Open Open Open
                                                           , Corners OneSide Open OneSide Complete
                                                           , Corners Open Open Open Open ]
        actual :: Raster B p 2 2 Corners
        actual = strict B . fpartition . fromRight . fromVector Seq $ U.fromList ([1,1,2,1] :: [Int])

fshapeTest :: Assertion
fshapeTest = actual @?= expected
 where expected :: Raster B p 3 3 Corners
       expected = fromRight . fromVector Seq $ V.fromList [ Corners Open Open OutFlow Open
                                                          , Corners Open Open Open Open
                                                          , Corners Open OutFlow Open Open
                                                          , Corners Open Open Open Open
                                                          , Corners Complete Complete Complete Complete
                                                          , Corners Open Open Open Open
                                                          , Corners Open Open Open OutFlow
                                                          , Corners Open Open Open Open
                                                          , Corners OutFlow Open Open Open ]
       actual :: Raster B p 3 3 Corners
       actual = strict B . fshape . fromRight . fromVector Seq $ U.fromList ([1,1,1,1,0,1,1,1,1] :: [Int])

ffrontageTest :: Assertion
ffrontageTest = let ?epsilon = 0.001 in actual @?~ expected
  where expected :: Double
        expected = 1 + (1 / sqrt 2)
        actual :: Double
        actual = flip index' (1 :. 1) . _array . strict S $ ffrontage rast
        rast :: Raster DW p 4 4 Corners
        rast = fshape . fromRight . fromVector Seq $ U.fromList ( [1,1,1,0
                                                                  ,1,0,0,0
                                                                  ,1,0,0,1
                                                                  ,1,0,1,1] :: [Int] )

fareaOpen :: Assertion
fareaOpen = actual @?= expected
  where expected :: Raster U p 3 3 Double
        expected = fromRight . fromVector Seq $ U.fromList [1,1,1,1,1,1,1,1,1]
        actual :: Raster U p 3 3 Double
        actual = strict U . farea . fshape . fromRight . fromVector Seq $ U.fromList ([0,0,0,0,0,0,0,0,0] :: [Int])

fareaCentre :: Assertion
fareaCentre = actual @?= expected
  where expected :: Raster U p 3 3 Double
        expected = fromRight . fromVector Seq $ U.fromList [ 1 + 1/8, 1, 1 + 1/8
                                                           , 1, 1/2, 1
                                                           , 1 + 1/8, 1, 1 + 1/8 ]
        actual :: Raster U p 3 3 Double
        actual = strict U . farea . fshape . fromRight . fromVector Seq $ U.fromList ([0,0,0,0,1,0,0,0,0] :: [Int])

fareaComplex :: Assertion
fareaComplex = let ?epsilon = 0.001 in actual @?~ (7 / 8)
  where actual :: Double
        actual = flip index' (1 :. 1) . _array . strict P $ farea rast
        rast :: Raster DW p 4 4 Corners
        rast = fshape . fromRight . fromVector Seq $ U.fromList ( [1,1,1,0
                                                                  ,1,0,0,0
                                                                  ,1,0,0,1
                                                                  ,1,0,1,1] :: [Int] )

fvolumeFlat :: Assertion
fvolumeFlat = (strict U $ fvolume expected) @?= expected
  where expected :: Raster U p 3 3 Double
        expected = fromRight . fromVector Seq $ U.fromList [8,8,8,8,8,8,8,8,8]

fvolumeHill :: Assertion
fvolumeHill = (flip index' (1 :. 1) $ _array actual) @?= expected
  where expected :: Double
        expected = P.sum [20,20,16,20,16,16,16,16,12,16,12,12] / 12
        actual :: Raster U p 3 3 Double
        actual = strict U . fvolume @Double . fromRight . fromVector Seq $ U.fromList [24,24,24
                                                                                      ,16,16,16
                                                                                      ,8,8,8]

newtype Vec = Vec [Double] deriving (Show)

instance Arbitrary Vec where
  arbitrary = Vec <$> QC.vector 9

-- | A QuickCheck property to test whether my custom Least Squares is as
-- accurate as the one provided by HMatrix.
leastSquares :: Vec -> Bool
leastSquares (Vec vs) = f 0 && f 1 && f 2
  where m = head . LA.toColumns $ LA.linearSolveLS zing (LA.col vs)
        v = leftPseudo LA.#> LA.vector vs
        f i = (m LA.! i) =~ (v LA.! i)

-- | Approximate Equality.
(=~) :: Double -> Double -> Bool
a =~ b = abs (a - b) < 0.0001

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

fgradientFlat :: Assertion
fgradientFlat = actual @?= expected
  where expected :: Raster U p 3 3 Double
        expected = fromRight . fromVector Seq $ U.fromList [0,0,0,0,0,0,0,0,0]
        actual :: Raster U p 3 3 Double
        actual = strict U . fgradient . fromRight . fromVector Seq $ U.fromList ([1,1,1,1,1,1,1,1,1] :: [Double])

fgradient45 :: Assertion
fgradient45 = let ?epsilon = 0.0001 in (flip index' (1 :. 1) $ _array actual) @?~ (tau / 8)
  where actual :: Raster U p 3 3 Double
        actual = strict U . fgradient . fromRight . fromVector Seq $ U.fromList ([3,3,3,2,2,2,1,1,1] :: [Double])

faspectFlat :: Assertion
faspectFlat = (flip index' (1 :. 1) $ _array actual) @?= Nothing
  where actual :: Raster B p 3 3 (Maybe Double)
        actual = strict B . faspect . fromRight . fromVector Seq $ U.fromList ([1,1,1,1,1,1,1,1,1] :: [Double])

faspect45 :: Assertion
faspect45 = (flip index' (1 :. 1) $ _array actual) @?= Just (tau / 2)
  where actual :: Raster B p 3 3 (Maybe Double)
        actual = strict B . faspect . fromRight . fromVector Seq $ U.fromList ([3,3,3,2,2,2,1,1,1] :: [Double])

faspectEast :: Assertion
faspectEast = let ?epsilon = 0.0001 in (flip index' (1 :. 1) $ _array actual) @?~ (tau / 4)
  where actual :: Raster B p 3 3 Double
        actual = strict B . faspect' . fromRight . fromVector Seq $ U.fromList ([3,2,1,3,2,1,3,2,1] :: [Double])

fdownstream4 :: Assertion
fdownstream4 = (flip index' (1 :. 1) $ _array actual) @?= drainage (S.fromList [North,South,East,West])
  where actual :: Raster S p 3 3 Drain
        actual = strict S . fdownstream . fromRight . fromVector Seq $ U.fromList ([3,1,3,1,2,1,3,1,3] :: [Double])

fdownstreamFlat :: Assertion
fdownstreamFlat = (flip index' (1 :. 1) $ _array actual) @?= drainage (S.fromList [East ..])
  where actual :: Raster S p 3 3 Drain
        actual = strict S . fdownstream . fromRight . fromVector Seq $ U.fromList ([1,1,1,1,1,1,1,1,1] :: [Double])

fdownstreamPeak :: Assertion
fdownstreamPeak = (flip index' (1 :. 1) $ _array actual) @?= drainage (S.fromList [NorthEast, NorthWest, SouthWest, SouthEast])
  where actual :: Raster S p 3 3 Drain
        actual = strict S . fdownstream . fromRight . fromVector Seq $ U.fromList ([1,1,1,1,3,1,1,1,1] :: [Double])

fdownstreamPit :: Assertion
fdownstreamPit = (flip index' (1 :. 1) $ _array actual) @?= Drain 0
  where actual :: Raster S p 3 3 Drain
        actual = strict S . fdownstream . fromRight . fromVector Seq $ U.fromList ([2,2,2,2,1,2,2,2,2] :: [Double])

fupstreamFlat :: Assertion
fupstreamFlat = (flip index' (1 :. 1) $ _array actual) @?= drainage (S.fromList [East ..])
  where actual :: Raster S p 3 3 Drain
        actual = strict S . fupstream . strict S . fdownstream . fromRight . fromVector Seq $ U.fromList ([1,1,1,1,1,1,1,1,1] :: [Double])

fupstreamPeak :: Assertion
fupstreamPeak = (flip index' (1 :. 1) $ _array actual) @?= Drain 0
  where actual :: Raster S p 3 3 Drain
        actual = strict S . fupstream . strict S . fdownstream . fromRight . fromVector Seq $ U.fromList ([1,1,1,1,3,1,1,1,1] :: [Double])
