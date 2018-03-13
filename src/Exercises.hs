module Execises where

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x:xs) = (x:xs) : suffixes xs
