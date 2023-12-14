import Data.Functor
import qualified Data.Set as S

matchingNumbers :: String -> Int
matchingNumbers s =
    let nums = drop 2 $ words s
        w = S.fromList $ takeWhile (/= "|") nums
        m = S.fromList $ drop 1 $ dropWhile (/= "|") nums
     in S.size $ S.intersection w m

scoreCard :: String -> Int
scoreCard s = let matches = matchingNumbers s in if matches == 0 then 0 else 2 ^ (matches -1)

part1 :: IO Int
part1 = readFile "/home/elliotbobrow/Downloads/day4" <&> sum . map scoreCard . lines

part2 :: IO Int
part2 = readFile "/home/elliotbobrow/Downloads/day4" <&> fst . foldl f (0, repeat 1) . lines
  where
    f (score, amts) x =
        let score' = matchingNumbers x
         in ( score + head amts
            , zipWith (+) (replicate score' (head amts) ++ repeat 0) (tail amts)
            )
