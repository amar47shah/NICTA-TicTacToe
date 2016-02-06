module TicTacToe where

import Data.List (intersperse)

type Game = Either Unfinished Finished

data Finished = Finished Board
data Unfinished = Unfinished Board

data Board = Board { aa, ab, ac, ba, bb, bc, ca, cb, cc :: Cell }

instance Show Board where
  show (Board aa ab ac ba bb bc ca cb cc) =
      unlines
    . intersperse "--|---|--"
    . map (concat . intersperse " | " . map show)
    $ [ [aa, ab, ac]
      , [ba, bb, bc]
      , [ca, cb, cc]
      ]

data Cell = Claimed Mark | Unclaimed

instance Show Cell where
  show (Claimed m) = show m
  show Unclaimed   = " "

data Mark = X | O

instance Show Mark where
  show X = "X"
  show O = "O"

type Position = (Coordinate, Coordinate)

data Coordinate = A | B | C

-- this is wrong because
-- 1) always returns an unfinished game
-- 2) allows a new move to occupy an already claimed cell
move :: Mark -> Unfinished -> Position -> Game
move m (Unfinished b) p =
  case p of
    (A, A) -> Left $ Unfinished b { aa = Claimed m }
    (A, B) -> Left $ Unfinished b { ab = Claimed m }
    (A, C) -> Left $ Unfinished b { ac = Claimed m }
    (B, A) -> Left $ Unfinished b { ba = Claimed m }
    (B, B) -> Left $ Unfinished b { bb = Claimed m }
    (B, C) -> Left $ Unfinished b { bc = Claimed m }
    (C, A) -> Left $ Unfinished b { ca = Claimed m }
    (C, B) -> Left $ Unfinished b { cb = Claimed m }
    (C, C) -> Left $ Unfinished b { cc = Claimed m }
