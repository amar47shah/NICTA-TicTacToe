module Main where

import Game (Game, Position, Coordinate, move, isFinished, start, bounds)
import Opponent (randomTurn)

import Control.Monad (join)
import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main =
  printLine >>
    untilM2 isFinished (randomTurn, takeTurn) start >>=
      print >>
        pure ()

takeTurn :: Game -> IO Game
takeTurn game = untilM (/= game) tryTurn game

tryTurn :: Game -> IO Game
tryTurn game =
  print game >>
    getPosition >>= \pos ->
      printLine >>
        pure (game >>= move pos)

getPosition :: IO Position
getPosition = (,) <$> getCoord "ROW" <*> getCoord "COLUMN"

getCoord :: String -> IO Coordinate
getCoord p = fromJust <$> untilM isCoord (const $ promptCoord p) Nothing

isCoord :: Maybe Coordinate -> Bool
isCoord (Just c) = c `elem` bounds
isCoord _        = False

promptCoord :: String -> IO (Maybe Coordinate)
promptCoord p = prompt p >> fmap readMaybe getLine

prompt :: String -> IO ()
prompt p = putStr (padRight 8 (p ++ ": ")) >> hFlush stdout

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

printLine :: IO ()
printLine = putStrLn "\n"

untilM2 :: Monad m => (a -> Bool) -> (a -> m a, a -> m a) -> a -> m a
untilM2 p (k, l) x
 | p x       = pure x
 | otherwise = k x >>= untilM2 p (l, k)

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p = untilM2 p . join (,)
