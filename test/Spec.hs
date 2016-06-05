module Main where

import Game.Internal

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
  , HU.testCase "-1 `elem` bounds == False" $ -1 `elem` bounds HU.@?= False
  ]

props :: TestTree
props = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "QuickCheck"
  [ QC.testProperty "game starts empty"              propGameStartsEmpty
  , QC.testProperty "one move doesn't finish a game" propOneMoveGameUnfinished
  , QC.testProperty "first move claims a cell for X" propFirstMoveForX
  , QC.testProperty "second turn for O"              propSecondTurnForO
  , QC.testProperty "can't move to claimed cell"     propNoMoveToClaimed
  ]

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

propNoMoveToClaimed :: QC.Property
propNoMoveToClaimed =
  QC.forAll pos $
    \p -> let Right (Unfinished _ pl) = start >>= move p >>= move p in pl == O

coord :: QC.Gen Coordinate
coord = QC.choose (lower, upper)

pos :: QC.Gen Position
pos = (,) <$> coord <*> coord

player :: QC.Gen Player
player = QC.elements [X, O]

cell :: QC.Gen Cell
cell = QC.oneof [pure Unclaimed, Claimed <$> player]

straight :: QC.Gen Straight
straight = QC.listOf cell

board :: QC.Gen Board
board = boardFromCells <$> QC.infiniteListOf cell

unfinished :: QC.Gen Unfinished
unfinished = Unfinished <$> board <*> player

outcome :: QC.Gen Outcome
outcome = QC.oneof [pure Draw, Won <$> player]

finished :: QC.Gen Finished
finished = Finished <$> board <*> outcome

game :: QC.Gen Game
game = QC.oneof [Left <$> finished, Right <$> unfinished]
