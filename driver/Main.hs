module Main where

import Game (Game, Position, Coordinate, move, isFinished, start, bounds)

import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main =
  printLine >>
    untilM isFinished takeTurn start >>=
      print >>
        pure ()

takeTurn :: Game -> IO Game
takeTurn game =
  print game >>
    getPosition >>= \pos ->
      printLine >>
        pure (game >>= move pos)

getPosition :: IO Position
getPosition = (,) <$> getCoord "ROW" <*> getCoord "COLUMN"

getCoord :: String -> IO Coordinate
getCoord p = untilM (`elem` bounds) (const $ promptCoord p) badCoord

promptCoord :: String -> IO Coordinate
promptCoord p = prompt p >> fmap readCoord getLine

readCoord :: String -> Coordinate
readCoord = fromMaybe badCoord . readMaybe

badCoord :: Coordinate
badCoord = -1

prompt :: String -> IO ()
prompt p = putStr (padRight 8 (p ++ ": ")) >> hFlush stdout

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

printLine :: IO ()
printLine = putStrLn "\n"

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p k x
 | p x       = pure x
 | otherwise = k x >>= untilM p k
