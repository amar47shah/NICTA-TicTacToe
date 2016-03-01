module Main where

import Game (Game, Position, Coordinate, move, isFinished, start, bounds)

import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main =
  putStrLn "" >>
    untilM isFinished takeTurn start >>=
      putStrLn . show >>
        return ()

takeTurn :: Game -> IO Game
takeTurn game =
  (putStrLn . show) game >>
    getPosition >>=
      (putStrLn "\n" >>) . return . (game >>=) . move

getPosition :: IO Position
getPosition = (,) <$> getCoord "ROW" <*> getCoord "COLUMN"

getCoord :: String -> IO Coordinate
getCoord p =
  untilM (`elem` bounds) (\_ -> prompt p >> fmap readCoord getLine) badCoord

readCoord :: String -> Coordinate
readCoord = fromMaybe badCoord . readMaybe

badCoord :: Coordinate
badCoord = -1

prompt :: String -> IO ()
prompt = (>> hFlush stdout) . putStr . (++ ": ")

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p k x
 | p x       = return x
 | otherwise = k x >>= untilM p k
