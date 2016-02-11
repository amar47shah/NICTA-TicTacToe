module Main where

import TicTacToe

import Data.Char (digitToInt)

main :: IO ()
main = do
  let game = start
  putStrLn $ show game
  putStr "Your move."
  position <- (,) <$> (putStr "\nROW: " >> digitToInt <$> getChar) <*> (putStr "\nCOLUMN: " >> digitToInt <$> getChar)
  putStrLn ""
  let game' = game >>= move position
  putStrLn $ show game'
