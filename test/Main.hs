module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain qcProps

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "myLength xs == length xs" $
      \xs -> myLength (xs :: [Int]) == length xs
  ]

myLength :: [a] -> Int
myLength = foldr (const succ) 0
