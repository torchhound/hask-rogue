module Lib
    ( parse,
	  initMap
    ) where

import qualified Data.Map as Map

initMap :: Map.Map String Integer
initMap = Map.fromList[("Spawn",1),("Dull Dungeon",1)]

parse :: Map.Map String Integer-> IO ()
parse map = do
	line <- getLine
	putStrLn line
	print map
	parse map
