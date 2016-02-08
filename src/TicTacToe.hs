{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TicTacToe (Mark (..), start, move, playerAt, isDraw, whoWon, sample) where

import Data.Array (Array, (!), (//), elems, listArray)
import Data.List (find, intercalate, intersperse)
import Data.Maybe (isNothing)

-- Exported definitions

data Mark = X | O deriving (Eq, Show)

start :: Game
start = Right $ Unfinished empty X

move :: Position -> Unfinished -> Game
move p (Unfinished b m) = game $ move' p m b

playerAt :: Position -> Board -> Cell
playerAt p b = b ! p

isDraw :: Finished -> Bool
isDraw = isNothing . whoWon

whoWon :: Finished -> Winner
whoWon (Finished _ w) = w

sample :: Game
sample =  start >>=
  move (1, 1) >>=
  move (2, 2) >>=
  move (3, 1) >>=
  move (2, 1) >>=
  move (2, 3) >>=
  move (1, 2) >>=
  move (3, 2) >>=
  move (3, 3) >>=
  move (1, 3)

-- Private definitions

type Game = Either Finished Unfinished
data Finished = Finished Board Winner
data Unfinished = Unfinished Board Mark
type Winner = Maybe Mark
type Board = Array Position Cell
type Position = (Coordinate, Coordinate)
type Coordinate = Int
type Straight = [Cell]
data Cell = Unclaimed | Claimed Mark

instance {-# OVERLAPPING #-} Show Game where
  show = either show show

instance Show Finished where
  show (Finished b Nothing)  = "DRAW\n" ++ show b
  show (Finished b (Just m)) = "WINNER: " ++ show m ++ '\n' : show b

instance Show Unfinished where
  show (Unfinished b m) = show m ++ " to play\n" ++ show b

instance {-# OVERLAPPING #-} Show Board where
  show = unlines
       . intersperse bar
       . (intercalate " | " <$>)
       . ((show <$>) <$>)
       . rows
    where bar :: String
          bar = concat $ "-" : replicate (pred dim) "-|--"

instance Show Cell where
  show (Claimed m) = show m
  show _           = " "

dim :: Coordinate
dim = 3

move' :: Position -> Mark -> Board -> Unfinished
move' p m b =
  case b ! p of
    Unclaimed -> Unfinished (b // [(p, Claimed m)]) $ opposite m
    _         -> Unfinished b m

opposite :: Mark -> Mark
opposite X = O
opposite O = X

game :: Unfinished -> Game
game u@(Unfinished b _)
 | isWon  b  = Left . Finished b $ winner b
 | isFull b  = Left $ Finished b Nothing
 | otherwise = Right u

isWon :: Board -> Bool
isWon = any isAllClaimed . straights

isAllClaimed :: Straight -> Bool
isAllClaimed s = isAllClaimedBy X s || isAllClaimedBy O s

isAllClaimedBy :: Mark -> Straight -> Bool
isAllClaimedBy m = all (isClaimedBy m)

isClaimedBy :: Mark -> Cell -> Bool
isClaimedBy _ Unclaimed    = False
isClaimedBy m (Claimed m') = m == m'

isClaimed :: Cell -> Bool
isClaimed Unclaimed = False
isClaimed _         = True

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
isFull = all isClaimed . elems

winner :: Board -> Winner
winner b =
  find isAllClaimed (straights b) >>=
    (\s -> if isAllClaimedBy X s then Just X else Just O)

empty :: Board
empty = listArray ((1, 1), (dim, dim)) $ repeat Unclaimed
