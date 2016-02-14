{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TicTacToe ( start
                 , move
                 , playerAt
                 , isFinished
                 , isDraw
                 , whoWon
                 , sample
                 , Game
                 , Position
                 ) where

import Data.Array (Array, (!), (//), elems, listArray)
import Data.Either (isLeft)
import Data.List (find, intercalate, intersperse)
import Data.Maybe (isNothing)

-- Exported definitions

start :: Game
start = Right $ Unfinished empty X

move :: Position -> Unfinished -> Game
move p (Unfinished b m) = game $ move' p m b

playerAt :: Position -> Game -> Maybe Cell
playerAt p (Left  (Finished   b _)) = b !? p
playerAt p (Right (Unfinished b _)) = b !? p

isFinished :: Game -> Bool
isFinished = isLeft

isDraw :: Finished -> Bool
isDraw = isNothing . whoWon

whoWon :: Finished -> Winner
whoWon (Finished _ w) = w

sample :: Game
sample = start >>=
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
data Cell = Unclaimed | Claimed Mark deriving Show
data Mark = X | O deriving (Eq, Show)

instance {-# OVERLAPPING #-} Show Game where
  show = either show show

instance Show Finished where
  show (Finished b Nothing)  = show b ++ "\nDRAW"
  show (Finished b (Just m)) = show b ++ "\nWINNER: " ++ show m

instance Show Unfinished where
  show (Unfinished b m) = show b ++ "\n" ++ show m ++ " to play"

instance {-# OVERLAPPING #-} Show Board where
  show = intercalate "\n"
       . intersperse bar
       . (intercalate " | " <$>)
       . ((draw <$>) <$>)
       . rows
    where bar :: String
          bar = concat $ "-" : replicate (upper - lower) "-|--"
          draw :: Cell -> String
          draw (Claimed m) = show m
          draw _           = " "

empty :: Board
empty = listArray ((lower, lower), (upper, upper)) $ repeat Unclaimed

lower, upper :: Coordinate
(lower, upper) = (1, 3)

bounds :: [Coordinate]
bounds = [lower..upper]

inBounds :: Coordinate -> Bool
inBounds c = c `elem` bounds

(!?) :: Board -> Position -> Maybe Cell
b !? (m, n)
 | all inBounds [m, n] = Just $ b ! (m, n)
 | otherwise           = Nothing

move' :: Position -> Mark -> Board -> Unfinished
move' p m b =
  case b !? p of
    Just Unclaimed -> Unfinished (b // [(p, Claimed m)]) $ opposite m
    _              -> Unfinished b m

opposite :: Mark -> Mark
opposite X = O
opposite O = X

game :: Unfinished -> Game
game u@(Unfinished b _)
 | isWon  b  = Left . Finished b $ winner b
 | isFull b  = Left $ Finished b Nothing
 | otherwise = Right u

isFull :: Board -> Bool
isFull = all isClaimed . elems

isWon :: Board -> Bool
isWon = any isAllClaimed . straights

isAllClaimed :: Straight -> Bool
isAllClaimed s = isAllClaimedBy X s || isAllClaimedBy O s

isAllClaimedBy :: Mark -> Straight -> Bool
isAllClaimedBy = all . isClaimedBy

isClaimedBy :: Mark -> Cell -> Bool
isClaimedBy _ Unclaimed    = False
isClaimedBy m (Claimed m') = m == m'

isClaimed :: Cell -> Bool
isClaimed Unclaimed = False
isClaimed _         = True

winner :: Board -> Winner
winner b =
  find isAllClaimed (straights b) >>=
    (\s -> if isAllClaimedBy X s then Just X else Just O)

straights :: Board -> [Straight]
straights b = concatMap ($ b) [rows, columns, diagonals]

rows :: Board -> [Straight]
rows b =
  [ [ b ! (m, n) | n <- bounds ] | m <- bounds ]

columns :: Board -> [Straight]
columns b =
  [ [ b ! (m, n) | m <- bounds ] | n <- bounds ]

diagonals :: Board -> [Straight]
diagonals b =
  [ [ b ! (m, n) | m <- bounds, n <- bounds, m == n ]
  , [ b ! (m, n) | m <- bounds, n <- bounds, m + n == lower + upper ]
  ]
