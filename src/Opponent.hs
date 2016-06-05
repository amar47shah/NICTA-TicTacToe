module Opponent (randomTurn) where

import Game (Game, Unfinished, Position, move, openPositions)

import System.Random (getStdRandom, randomR)

randomTurn :: Game -> IO Game
randomTurn = either (pure . Left) randomMove

randomMove :: Unfinished -> IO Game
randomMove u = move <$> randomPosition u <*> pure u

randomPosition :: Unfinished -> IO Position
randomPosition = pick . openPositions

pick :: [a] -> IO a
pick [] = error "cannot pick from empty list"
pick xs = (xs !!) <$> (getStdRandom . randomR) (0, pred $ length xs)
