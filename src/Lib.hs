module Lib
  ( parse,
    randomInt,
    initMap,
    PlayerPosition(PlayerPosition),
    replaceXY
  ) where

import System.Random

data PlayerPosition = PlayerPosition {ppx :: Int, ppy :: Int} deriving(Show)

randomInt :: StdGen -> Int -> (Int, StdGen)
randomInt gen mx = (randomR (1, mx) gen) 

initMap :: Int -> Int -> Int -> [[Char]] -> [Char] -> StdGen -> [[Char]]
initMap x y xConst map line gen
  | x == 0 = initMap xConst (y - 1) xConst (line:map) [] gen
  | y > 0 = let (rand, newGen) = (randomInt gen 20)
            in initMap (x - 1) y xConst map ((if rand >= 10 then '#' else '.'):line) newGen
  | y == 0 = map

replaceXY :: Int -> Int -> Char -> [[Char]] -> [[Char]]
replaceXY x y new map =
  let row = map!!y
      (rhd, rtl) = splitAt x row
      newRow = rhd ++ (new:(tail rtl))
      (mhd, mtl) = splitAt y map
  in mhd ++ (newRow:(tail mtl))

parse :: [[Char]] -> PlayerPosition -> IO ()
parse map pp = do
  print pp
  line <- getLine
  newPP <- case line of
    "n" -> do print $ "North"
              return (if (ppx pp) /= 0 then (PlayerPosition (ppx pp) ((ppy pp) - 1)) else pp)
    "s" -> do print $ "South"
              return (if (ppx pp) < 20 then (PlayerPosition (ppx pp) ((ppy pp) + 1)) else pp)
    "e" -> do print $ "East"
              return (if (ppx pp) < 20 then (PlayerPosition ((ppx pp) + 1) (ppy pp)) else pp)
    "w" -> do print $ "West"
              return (if (ppx pp) /= 0 then (PlayerPosition ((ppx pp) - 1) (ppy pp)) else pp)
    _ -> do print $ "Invalid Input"
            return pp
  print newPP
  let newMap = (replaceXY (ppx newPP) (ppy newPP) '*' map)
  (mapM print newMap)
  parse newMap newPP
