module Main where

import TicTacToe

import Test.Tasty
import Test.Tasty.HUnit as H

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ H.testCase "isFinished start == False" $ isFinished start  H.@?= False
  , H.testCase "isFinished sample == True" $ isFinished sample H.@?= True
  ]
