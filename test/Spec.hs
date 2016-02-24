module Main where

import TicTacToe

import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, props]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ HU.testCase "isFinished start == False" $ isFinished start  HU.@?= False
  , HU.testCase "isFinished sample == True" $ isFinished sample HU.@?= True
  ]

props :: TestTree
props = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "one move doesn't finish a game" .
                    QC.forAll pos $ (not . isFinished) . (start >>=) . move
  ]

coord :: QC.Gen Coordinate
coord = QC.choose (lower, upper)

pos :: QC.Gen Position
pos = (,) <$> coord <*> coord
