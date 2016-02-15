module Main where

import TicTacToe (Game, Position, Coordinate, move, isFinished, start)

import Data.Char (digitToInt)

main :: IO ()
main =
  putStrLn "" >>
    untilM isFinished takeTurn start >>=
      putStrLn . show >>
        return ()

takeTurn :: Game -> IO Game
takeTurn game =
  (putStr . show) game >> getPosition >>=
      (putStr "\n\n" >>) . return . (game >>=) . move

getPosition :: IO Position
getPosition = (,) <$> inputCoord "ROW" <*> inputCoord "COLUMN"
    where
  inputCoord :: String -> IO Coordinate
  inputCoord s = putStr ('\n' : s ++ ": ") >> toCoord <$> getChar
  toCoord :: Char -> Coordinate
  toCoord c | c `elem` ['0'..'9'] = digitToInt c
            | otherwise           = 0

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p k x
 | p x       = return x
 | otherwise = k x >>= untilM p k
