{-# LANGUAGE DataKinds #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Geography.MapAlgebra

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testCase "constant" $ length (constant 5 :: Raster p 256 256 Int) @?= 65536
  ]
