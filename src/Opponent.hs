module Opponent where

import Game.Internal

import System.Random (getStdRandom, randomR)

randomPosition :: IO Position
randomPosition = (,) <$> randomCoordinate <*> randomCoordinate

randomCoordinate :: IO Coordinate
randomCoordinate = getStdRandom . randomR $ (lower, upper)
