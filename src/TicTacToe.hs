module TicTacToe where

import Data.List (intersperse)

data Finished = Finished Board
data Unfinished = Unfinished Board

data Board = B { aa, ab, ac, ba, bb, bc, ca, cb, cc :: Cell }

instance Show Board where
  show (B aa ab ac ba bb bc ca cb cc) =
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
