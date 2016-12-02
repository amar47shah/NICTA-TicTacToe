module Main where

import Game (Game, Position, Coordinate, move, isFinished, start, bounds, marks)
import Opponent (randomMove)

import Control.Monad (join)
import Data.Maybe (fromJust, isJust)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main =
  putDoubleLine >>
    getTurns >>= \turns ->
      putDoubleLine >>
        untilM2 isFinished turns start >>=
          print >>
            pure ()

type Turn = Game -> IO Game
data PlayerType = H | C deriving Read

getTurns :: IO (Turn, Turn)
getTurns =
  putStrLn "(H)uman  or\n(C)omputer?" >>
      (,) <$> getTurn (fst marks) <*> getTurn (snd marks)

getTurn :: String -> IO Turn
getTurn m = getInput (turn . fromJust) isJust ("Player " ++ m)

turn :: PlayerType -> Turn
turn H = takeTurn
turn C = randomTurn

takeTurn :: Turn
takeTurn game = untilM (/= game) tryTurn game

tryTurn :: Turn
tryTurn game =
  print game >>
    getPosition >>= \pos ->
      putDoubleLine >>
        pure (game >>= move pos)

randomTurn :: Turn
randomTurn game =
  print game >>
    putDoubleLine >>
      either (pure . Left) randomMove game

getPosition :: IO Position
getPosition = (,) <$> getCoord "ROW" <*> getCoord "COLUMN"

getCoord :: String -> IO Coordinate
getCoord = getInput fromJust isCoord

isCoord :: Maybe Coordinate -> Bool
isCoord (Just c) = c `elem` bounds
isCoord _        = False

getInput :: Read a => (Maybe a -> b) -> (Maybe a -> Bool) -> String -> IO b
getInput t c p = t <$> untilM c (const $ promptLine p) Nothing

promptLine :: Read a => String -> IO (Maybe a)
promptLine p = prompt p >> fmap readMaybe getLine

prompt :: String -> IO ()
prompt p = putStr (padRight 8 (p ++ ": ")) >> hFlush stdout

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

putDoubleLine :: IO ()
putDoubleLine = putStrLn "\n"

untilM2 :: Monad m => (a -> Bool) -> (a -> m a, a -> m a) -> a -> m a
untilM2 p (k, l) x
 | p x       = pure x
 | otherwise = k x >>= untilM2 p (l, k)

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p = untilM2 p . join (,)
