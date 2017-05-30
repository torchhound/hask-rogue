module Main where

import Lib
import Control.Monad
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    parse (initMap 20 20 20 [] [] gen) (PlayerPosition 0 0) '.' gen
