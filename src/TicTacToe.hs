{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TicTacToe where

import Data.Array
import Data.List (intercalate, intersperse)

type Game = Either Finished Unfinished

data Finished = Finished Board Winner

type Winner = Maybe Mark

instance Show Finished where
  show (Finished b Nothing)  = "DRAW\n" ++ show b
  show (Finished b (Just m)) = "WINNER: " ++ show m ++ '\n' : show b

data Unfinished = Unfinished Board

instance Show Unfinished where
  show (Unfinished b) = "Unfinished\n" ++ show b

type Board = Array Position Cell

instance {-# OVERLAPPING #-} Show Board where
  show = unlines
       . intersperse bar
       . (intercalate " | " <$>)
       . splitEvery dim
       . (show <$>)
       . elems
    where bar = concat $ "-" : replicate (pred dim) "-|--"

type Position = (Coordinate, Coordinate)
type Coordinate = Int

dim :: Coordinate
dim = 3

data Cell = Claimed Mark | Unclaimed

instance Show Cell where
  show (Claimed m) = show m
  show Unclaimed   = " "

data Mark = X | O deriving Show

move :: Position -> Mark -> Unfinished -> Game
move p m (Unfinished b)
   | isFinished $ b' = Left  . Finished b' $ winner b' --inefficient, checks if finished and then again to get winner
   | otherwise       = Right $ Unfinished b'
  where b' = move' p m b

move' :: Position -> Mark -> Board -> Board
move' p m b =
  case b ! p of
    Unclaimed -> b // [(p, Claimed m)]
    Claimed _ -> b

--wrong, just a placeholder
isFinished :: Board -> Bool
isFinished = const False

--wrong, just a placeholder
winner :: Board -> Winner
winner = const $ Just X

start :: Game
start = Right $ Unfinished empty

empty :: Board
empty = listArray ((1, 1), (dim, dim)) $ repeat Unclaimed

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
  move (3, 3) O >>=
  move (1, 3) X
