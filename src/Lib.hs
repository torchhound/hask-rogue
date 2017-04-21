module Lib
  ( parse,
    initMap,
    randomInt
  ) where

import System.Random

randomInt :: StdGen -> Int -> (Int, StdGen)
randomInt gen mx = 
  (randomR (1, mx) gen) 

initMap :: Int -> Int -> Int -> [[Char]] -> [Char] -> StdGen -> [[Char]]
initMap x y xConst map line gen
  | x == 0 = let (rand, newGen) = (randomInt gen 20) 
             in initMap xConst (y - 1) xConst (line:map) ((if rand >= 10 then '#' else '.'):[]) newGen
  | y > 0 = let (rand, newGen) = (randomInt gen 20)
            in initMap (x - 1) y xConst map ((if rand >= 10 then '#' else '.'):line) newGen
  | y == 0 = map

parse :: [[Char]] -> IO ()
parse map = do
  line <- getLine
  putStrLn line
  (mapM print map)
  parse map
