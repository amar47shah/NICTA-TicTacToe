module Main where

import TicTacToe (Game, Position, move, isFinished, start)

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
getPosition = (,) <$> inputDigit "ROW" <*> inputDigit "COLUMN"
  where inputDigit s = putStr ('\n' : s ++ ": ") >> digitToInt <$> getChar

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p k x
 | p x       = return x
 | otherwise = k x >>= untilM p k
