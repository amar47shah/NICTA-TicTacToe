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
  [ QC.testProperty "game starts empty"              propGameStartsEmpty
  , QC.testProperty "one move doesn't finish a game" propOneMoveGameUnfinished
  , QC.testProperty "first move claims a cell for X" propFirstMoveForX
  , QC.testProperty "second turn for O"              propSecondTurnForO
  ]

coord :: QC.Gen Coordinate
coord = QC.choose (lower, upper)

pos :: QC.Gen Position
pos = (,) <$> coord <*> coord

propGameStartsEmpty :: QC.Property
propGameStartsEmpty =
  QC.forAll pos $ \p -> playerAt p start == Just Unclaimed

propOneMoveGameUnfinished :: QC.Property
propOneMoveGameUnfinished =
  QC.forAll pos $ not . isFinished . (start >>=) . move

propFirstMoveForX :: QC.Property
propFirstMoveForX =
  QC.forAll pos $ \p -> playerAt p (start >>= move p) == Just (Claimed X)

propSecondTurnForO :: QC.Property
propSecondTurnForO =
  QC.forAll pos $ \p -> (\(Right (Unfinished _ m)) -> m == O) (start >>= move p)
