{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Game.Internal where

import Data.Array (Array, (!), (//), elems, indices, listArray)
import Data.Either (isLeft)
import Data.List (find, intercalate, intersperse)

type Game       = Either Finished Unfinished
type Board      = Array Position Cell
type Position   = (Coordinate, Coordinate)
type Coordinate = Int
type Straight   = [Cell]

data Finished   = Finished Board Outcome
data Unfinished = Unfinished Board Player
data Outcome    = Draw | Won Player
data Cell       = Unclaimed | Claimed Player deriving (Eq, Show)
data Player     = X | O                      deriving (Eq, Show)

instance {-# OVERLAPPING #-} Show Game where
  show = either show show

instance Show Finished where
  show (Finished b o)   = show b ++ '\n' : show o

instance Show Unfinished where
  show (Unfinished b p) = show b ++ '\n' : show p ++ " to play"

instance {-# OVERLAPPING #-} Show Board where
  show = intercalate "\n"
       . intersperse bar
       . (intercalate " | " <$>)
       . ((mark <$>) <$>)
       . rows
    where bar :: String
          bar = concat $ "-" : replicate (upper - lower) "-|--"
          mark :: Cell -> String
          mark (Claimed p) = show p
          mark _           = "·"

instance Show Outcome where
  show Draw    = "DRAW"
  show (Won m) = "WINNER: " ++ show m

start :: Game
start = Right $ Unfinished empty X

move :: Position -> Unfinished -> Game
move = (next .) . move'

playerAt :: Position -> Game -> Maybe Cell
playerAt p (Left  (Finished   b _)) = b !? p
playerAt p (Right (Unfinished b _)) = b !? p

isFinished :: Game -> Bool
isFinished = isLeft

whoWon :: Finished -> Outcome
whoWon (Finished _ o) = o

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

lower, upper :: Coordinate
(lower, upper) = (1, 3)

boardFromCells :: [Cell] -> Board
boardFromCells = listArray ((lower, lower), (upper, upper))

empty :: Board
empty = boardFromCells $ repeat Unclaimed

bounds :: [Coordinate]
bounds = [lower..upper]

inBounds :: Position -> Bool
inBounds (m, n) = all (`elem` bounds) [m, n]

(!?) :: Board -> Position -> Maybe Cell
b !? p
 | inBounds p = Just $ b ! p
 | otherwise  = Nothing

move' :: Position -> Unfinished -> Unfinished
move' pos u@(Unfinished b p) = maybe u (place pos b p) (b !? pos)

place :: Position -> Board -> Player -> Cell -> Unfinished
place pos b p Unclaimed = Unfinished (b // [(pos, Claimed p)]) $ opposite p
place _   b p _         = Unfinished b p

opposite :: Player -> Player
opposite X = O
opposite O = X

openPositions :: Unfinished -> [Position]
openPositions (Unfinished b _) = filter (not . isClaimed . (b !)) $ indices b

next :: Unfinished -> Game
next u@(Unfinished b _)
 | isWon  b  = Left . Finished b $ winner b
 | isFull b  = Left $ Finished b Draw
 | otherwise = Right u

isFull :: Board -> Bool
isFull = all isClaimed . elems

isWon :: Board -> Bool
isWon = any isAllClaimed . straights

isAllClaimed :: Straight -> Bool
isAllClaimed s = isAllClaimedBy X s || isAllClaimedBy O s

isAllClaimedBy :: Player -> Straight -> Bool
isAllClaimedBy = all . isClaimedBy

isClaimedBy :: Player -> Cell -> Bool
isClaimedBy m (Claimed m') = m == m'
isClaimedBy _ _            = False

isClaimed :: Cell -> Bool
isClaimed Unclaimed = False
isClaimed _         = True

winner :: Board -> Outcome
winner = maybe Draw winner' . find isAllClaimed . straights
    where
  winner' (Claimed p:_) = Won p
  winner' (_        :_) = error "isAllClaimed straight contains unclaimed cells"
  winner' _             = error "isAllClaimed straight is empty"

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
