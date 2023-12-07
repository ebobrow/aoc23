{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char
import Data.Functor
import qualified Data.Map as Map

vals = Map.fromList [("red", 12), ("green", 13), ("blue", 14)]

chunk [] = []
chunk xs = (head xs, xs !! 1) : chunk (drop 2 xs)

part1 = readFile "/home/elliotbobrow/Downloads/day2" <&> sum . map lineimpossible . lines
  where
    lineimpossible =
        ((\l i -> if i then 0 else read $ l !! 1) <*> any impossible . chunk . drop 2)
            . words
            . filter (not . isPunctuation)
    impossible (n, c) = read n > vals Map.! c

part2 = readFile "/home/elliotbobrow/Downloads/day2" <&> sum . map (Map.foldr (*) 1 . countLine) . lines
  where
    countLine =
        foldl (\acc (n, c) -> Map.insertWith max c (read n) acc) Map.empty
            . (chunk . drop 2 . words . filter (not . isPunctuation))
