module Lib
    ( parse,
	  initMap,
      randomInt
    ) where

import System.Random

randomInt :: StdGen -> Int -> (Int, StdGen)
randomInt gen mx = 
    (randomR (1, mx) gen) 

initMap :: Int -> Int -> Int -> [Char] -> StdGen -> [Char]
initMap x y xConst map gen
    | x == 0 = initMap xConst (y - 1) xConst map gen
    | y > 0 = let (rand, newGen) = (randomInt gen 20)
              in initMap (x - 1) y xConst ((if rand >= 10 then '#' else '.'):map) newGen
    | y == 0 = map

parse :: [Char]-> IO ()
parse map = do
	line <- getLine
	putStrLn line
	print map
	parse map
