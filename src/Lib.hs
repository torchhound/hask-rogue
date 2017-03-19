module Lib
    ( parse
    ) where

parse :: IO ()
parse = do
	line <- getLine
	putStrLn line
