{-# LANGUAGE TupleSections #-}

-- "This is even worse than your day 1 code and this time it isn't even on purpose!" - Haters
-- :(

import Data.Char
import Data.Functor
import Data.List
import Data.Maybe

type Grid = [(Char, Integer, Integer)]

grid :: String -> Grid
grid input =
    concatMap (zipWith (\i (a, b) -> (a, b, i)) [1 ..]) $
        zipWith (\c i -> map (,i) c) (lines input) [1 ..]

punctuation :: Grid -> Grid
punctuation = filter (\(x, _, _) -> not (isDigit x) && x /= '.')

numFragments :: Grid -> Grid
numFragments grid =
    filter
        ( \(x, i, j) ->
            isDigit x
                && isJust
                    (findIndex (\(_, i', j') -> abs (i' - i) <= 1 && abs (j' - j) <= 1) $ punctuation grid)
        )
        grid

dedup :: (Ord a) => [a] -> [a]
dedup = map head . group . sort

fullNum :: [Grid] -> (Char, Integer, Integer) -> Grid
fullNum grid n =
    fromJust $ find (elem n) grid

groupedGrid :: Grid -> [Grid]
groupedGrid = groupBy (\(a, _, _) (b, _, _) -> isDigit a == isDigit b)

part1 :: IO Integer
part1 =
    readFile "/home/elliotbobrow/Downloads/day3"
        <&> sum
            . map (read . map (\(c, _, _) -> c))
            . dedup
            . (map . fullNum . groupedGrid <*> numFragments)
            . grid

gears :: Grid -> Grid
gears = filter (\(x, _, _) -> x == '*')

gearNumFragments :: Grid -> [Grid]
gearNumFragments grid =
    map
        ( \(_, i', j') ->
            filter (\(c, i, j) -> isDigit c && abs (i' - i) <= 1 && abs (j' - j) <= 1) grid
        )
        $ gears grid

part2 :: IO Integer
part2 =
    readFile "/home/elliotbobrow/Downloads/day3"
        <&> sum
            . map
                (product . map (read . map (\(c, _, _) -> c)))
            . filter ((> 1) . length)
            . map dedup
            . (map . map . fullNum . groupedGrid <*> gearNumFragments)
            . grid
