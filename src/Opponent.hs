module Opponent where

import Game.Internal

import System.Random (getStdRandom, randomR)

randomTurn :: Game -> IO Game
randomTurn g = (g >>=) <$> randomMove

randomMove :: IO (Unfinished -> Game)
randomMove = move <$> randomPosition

randomPosition :: IO Position
randomPosition = (,) <$> randomCoordinate <*> randomCoordinate

randomCoordinate :: IO Coordinate
randomCoordinate = getStdRandom . randomR $ (lower, upper)
