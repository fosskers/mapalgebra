{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import           Data.Int
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Massiv.Array as A
import qualified Data.Massiv.Array.Manifest.Vector as A
import qualified Data.Vector.Unboxed as U
import           Data.Word
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
    [ testCase "constant (256x256)"     $ length (lazy small) @?= 65536
    , testCase "constant (2^16 x 2^16)" $ length lazybig @?= 4294967296
    , testCase "Image Reading (RGBA)"   $ do
        i <- fileRGBA
        fmap (getComp . _array . _red) i @?= Right Par
    , testCase "Image Reading (Gray)"   $ do
        i <- fileY
        fmap (getComp . _array) i @?= Right Par
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

one :: Raster P p 7 7 Int
one = constant P Seq 1

two :: Raster P p 7 7 Int
two = constant P Seq 2

small :: Raster P p 256 256 Int
small = constant P Seq 5

lazybig :: Raster D p 65536 65536 Int
lazybig = constant D Par 5

big :: Raster P p 65536 65536 Word8
big = constant P Par 5

bog :: Raster P p 65536 65536 Word8
bog = constant P Par 10

-- | Should have two rows and 3 columns.
arr :: Array U Ix2 Int
arr = A.fromVector Seq (2 :. 3) $ U.fromList [0..5]

indices :: Raster D p 10 10 Int
indices = fromFunction D Seq (\(r :. c) -> (r * 10) + c)

fileRGBA :: IO (Either String (RGBARaster p 1753 1760 Word8))
fileRGBA = fromRGBA "/home/colin/code/haskell/mapalgebra/LC81750172014163LGN00_LOW5.TIF"

fileY :: IO (Either String (Raster D p 1753 1760 Word8))
fileY = fromGray "/home/colin/code/haskell/mapalgebra/LC81750172014163LGN00_LOW5.TIF"
