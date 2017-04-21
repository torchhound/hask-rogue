module Lib
  ( parse,
    initMap,
    randomInt,
    PlayerPosition(PlayerPosition)
  ) where

import System.Random

data PlayerPosition = PlayerPosition (Int, Int) deriving(Eq, Show)

randomInt :: StdGen -> Int -> (Int, StdGen)
randomInt gen mx = (randomR (1, mx) gen) 

initMap :: Int -> Int -> Int -> [[Char]] -> [Char] -> StdGen -> [[Char]]
initMap x y xConst map line gen
  | x == 0 = initMap xConst (y - 1) xConst (line:map) [] gen
  | y > 0 = let (rand, newGen) = (randomInt gen 20)
            in initMap (x - 1) y xConst map ((if rand >= 10 then '#' else '.'):line) newGen
  | y == 0 = map

parse :: [[Char]] -> PlayerPosition -> IO ()
parse map pp = do
  line <- getLine
  case line of
    "n" -> print $ "North"
    "s" -> print $ "South"
    "e" -> print $ "East"
    "w" -> print $ "West"
    _ -> print $ "Invalid Input"
  (mapM print map)
  parse map pp
