module Main where

import TicTacToe (Game, Position, Coordinate, move, isFinished, start)

import Data.Char (digitToInt)
import System.IO (hFlush, stdout)

main :: IO ()
main =
  putStrLn "" >>
    untilM isFinished takeTurn start >>=
      putStrLn . show >>
        return ()

takeTurn :: Game -> IO Game
takeTurn game =
  (putStrLn . show) game >> getPosition >>=
      (putStrLn "\n" >>) . return . (game >>=) . move

getPosition :: IO Position
getPosition =
  (,) <$> (prompt "ROW" *> getCoord) <*> (prompt "COLUMN" *> getCoord)
    where
  getCoord :: IO Coordinate
  getCoord = toCoord <$> getLine
  toCoord :: String -> Coordinate
  toCoord (c:"") | c `elem` ['0'..'9'] = digitToInt c
  toCoord _                            = -1

prompt :: String -> IO ()
prompt = (>> hFlush stdout) . putStr . (++ ": ")

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p k x
 | p x       = return x
 | otherwise = k x >>= untilM p k
