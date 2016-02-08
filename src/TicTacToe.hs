{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TicTacToe (Mark (..), start, move, playerAt, isDraw, whoWon, sample) where

import Data.Array (Array, (!), (//), elems, listArray)
import Data.List (find, intercalate, intersperse)
import Data.Maybe (fromMaybe, isJust, isNothing)

-- Exported definitions

data Mark = X | O deriving (Eq, Show)

start :: Game
start = Right $ Unfinished empty

move :: Position -> Mark -> Unfinished -> Game
move p m (Unfinished b) = game $ move' p m b

playerAt :: Position -> Board -> Cell
playerAt p b = b ! p

isDraw :: Finished -> Bool
isDraw = isNothing . whoWon

whoWon :: Finished -> Winner
whoWon (Finished _ w) = w

sample :: Game
sample =  start >>=
  move (1, 1) X >>=
  move (2, 2) O >>=
  move (3, 1) X >>=
  move (2, 1) O >>=
  move (2, 3) X >>=
  move (1, 2) O >>=
  move (3, 2) X >>=
  move (3, 3) O >>=
  move (1, 3) X

-- Private definitions

type Game = Either Finished Unfinished
data Finished = Finished Board Winner
data Unfinished = Unfinished Board
type Winner = Maybe Mark
type Board = Array Position Cell
type Position = (Coordinate, Coordinate)
type Coordinate = Int
type Cell = Maybe Mark
type Straight = [Cell]

instance {-# OVERLAPPING #-} Show Game where
  show = either show show

instance Show Finished where
  show (Finished b Nothing)  = "DRAW\n" ++ show b
  show (Finished b (Just m)) = "WINNER: " ++ show m ++ '\n' : show b

instance Show Unfinished where
  show (Unfinished b) = "Unfinished\n" ++ show b

instance {-# OVERLAPPING #-} Show Board where
  show = unlines
       . intersperse bar
       . (intercalate " | " <$>)
       . (drawCells <$>)
       . rows
    where bar :: String
          bar = concat $ "-" : replicate (pred dim) "-|--"
          drawCells :: Straight -> [String]
          drawCells = (maybe " " show <$>)

dim :: Coordinate
dim = 3

move' :: Position -> Mark -> Board -> Board
move' p m b =
  case b ! p of
    Nothing -> b // [(p, Just m)]
    _       -> b

game :: Board -> Game
game b
 | isWon  b  = Left . Finished b $ winner b
 | isFull b  = Left $ Finished b Nothing
 | otherwise = Right $ Unfinished b

isWon :: Board -> Bool
isWon = any isClaimed . straights

isClaimed :: Straight -> Bool
isClaimed s = isClaimedBy X s || isClaimedBy O s

isClaimedBy :: Mark -> Straight -> Bool
isClaimedBy m = fromMaybe False . (all (== m) <$>) . sequence

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
winner b =
  find isClaimed (straights b) >>=
    (\s -> if isClaimedBy X s then Just X else Just O)

empty :: Board
empty = listArray ((1, 1), (dim, dim)) $ repeat Nothing
