{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TicTacToe where

import Control.Applicative (liftA2)
import Control.Arrow ((***), (&&&))
import Control.Monad (join)
import Data.Array (Array, (!), (//), elems, listArray)
import Data.List (find, intercalate, intersperse)
import Data.Maybe (fromMaybe, isJust, listToMaybe)

type Game = Either Finished Unfinished
data Finished = Finished Board Winner
data Unfinished = Unfinished Board
type Winner = Maybe Mark
type Board = Array Position Cell
type Position = (Coordinate, Coordinate)
type Coordinate = Int
type Cell = Maybe Mark
type Straight = [Cell]
data Mark = X | O deriving (Eq, Show)

instance Show Finished where
  show (Finished b Nothing)  = "DRAW\n" ++ show b
  show (Finished b (Just m)) = "WINNER: " ++ show m ++ '\n' : show b

instance Show Unfinished where
  show (Unfinished b) = "Unfinished\n" ++ show b

instance {-# OVERLAPPING #-} Show Board where
  show = unlines
       . intersperse bar
       . (intercalate " | " <$>)
       . splitEvery dim
       . (draw <$>)
       . elems
    where bar = concat $ "-" : replicate (pred dim) "-|--"

draw :: Cell -> String
draw (Just m) = show m
draw _        = " "

dim :: Coordinate
dim = 3

move :: Position -> Mark -> Unfinished -> Game
move p m (Unfinished b) = game $ move' p m b

move' :: Position -> Mark -> Board -> Board
move' p m b =
  case b ! p of
    Nothing -> b // [(p, Just m)]
    _       -> b

game :: Board -> Game
game b
 | isWon  b   = Left . Finished b $ winner b
 | isFull b   = Left $ Finished b Nothing
 | otherwise  = Right $ Unfinished b

isWon :: Board -> Bool
isWon = any same . straights

same :: Straight -> Bool
same = fromMaybe False
     . uncurry (liftA2 (||))
     . ((all (== X) <$>) *** (all (== O) <$>))
     . (sequence &&& sequence)

straights :: Board -> [Straight]
straights b = concatMap ($ b) [rows, columns, diagonals]

rows :: Board -> [Straight]
rows b =
  [ [ b ! (m, n) | n <- [1..dim] ] | m <- [1..dim] ]

columns :: Board -> [Straight]
columns b =
  [ [ b ! (m, n) | m <- [1..dim] ] | n <- [1..dim] ]

diagonals :: Board -> [Straight]
diagonals b =
  [ [ b ! (m, n) | m <- [1..dim], n <- [1..dim], m == n ]
  , [ b ! (m, n) | m <- [1..dim], n <- [1..dim], m + n == 1 + dim ]
  ]

isFull :: Board -> Bool
isFull = all isJust . elems

winner :: Board -> Winner
winner = join . join . (listToMaybe <$>) . find same . straights

start :: Game
start = Right $ Unfinished empty

empty :: Board
empty = listArray ((1, 1), (dim, dim)) $ repeat Nothing

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
      where
  (first, rest) = splitAt n xs

sample :: Game
sample =  start >>=
  move (1, 1) X >>=
  move (2, 2) O >>=
  move (3, 1) X >>=
  move (2, 1) O >>=
  move (2, 3) X >>=
  move (1, 2) O >>=
  move (3, 2) X >>=
  move (1, 3) O >>=
  move (3, 3) X
