module Lib
  ( parse,
    randomInt,
    initMap,
    PlayerPosition(PlayerPosition),
    replaceXY
  ) where

import System.Random
import System.Exit

data PlayerPosition = PlayerPosition {ppx :: Int, ppy :: Int} deriving(Show)

randomInt :: StdGen -> Int -> (Int, StdGen)
randomInt gen mx = (randomR (1, mx) gen) 

initMap :: Int -> Int -> Int -> [[Char]] -> [Char] -> StdGen -> [[Char]]
initMap x y xConst map line gen
  | x == 0 = initMap xConst (y - 1) xConst (line:map) [] gen
  | y > 0 = let (rand, newGen) = (randomInt gen 20)
            in initMap (x - 1) y xConst map ((if rand >= 16 then '#' else if rand == 15 then 'E' else '.'):line) newGen
  | y == 0 = map

replaceXY :: Int -> Int -> Char -> [[Char]] -> [[Char]]
replaceXY x y new map =
  let row = map!!y
      (rhd, rtl) = splitAt x row
      newRow = rhd ++ (new:(tail rtl))
      (mhd, mtl) = splitAt y map
  in mhd ++ (newRow:(tail mtl))

getXY :: Int -> Int -> [[Char]] -> Char
getXY x y map =
  let row = map!!y
  in row!!x

parse :: [[Char]] -> PlayerPosition -> Char -> StdGen -> IO ()
parse map pp olderPP gen = do
  let (rand, newGen) = (randomInt gen 20)
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
  if (getXY (ppx newPP) (ppy newPP) map) == '#'
    then 
      do
        print $ "That's a wall!"
        parse map pp olderPP newGen
    else if (getXY (ppx newPP) (ppy newPP) map) == 'E'
      then
        do
          if rand > 10 
            then
              do
                print $ "Enemy defeated!" 
                let enemyMap = (replaceXY (ppx newPP) (ppy newPP) '.' map)
                parse enemyMap pp olderPP newGen
            else die $ "You lose..."
      else
        do
          let oldMap = (replaceXY (ppx pp) (ppy pp) olderPP map)
          let oldPP = (getXY (ppx newPP) (ppy newPP) map)
          let newMap = (replaceXY (ppx newPP) (ppy newPP) 'P' oldMap)
          (mapM print newMap)
          parse newMap newPP oldPP newGen