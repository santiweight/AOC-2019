-- Much of this solution is inspired by blinry's solution here:
-- https://github.com/blinry/advent-of-code-2019/blob/master/Day03.hs
-- Especiially the use of M.intersectionWith and scanl1
-- foldr1/scanl1 are also patterns that I amm glad to have learned
-- His solution really helped me with writing good Haskell

import Data.List.Split
import Data.Set as S
import Data.Map as M

import Util

import Linear

parseSteps :: String -> [V2 Int]
parseSteps (dirChr:lenStr) =
  replicate (read lenStr :: Int) $
  case dirChr of
                   'U' -> V2 0      1
                   'D' -> V2 0      (-1)
                   'L' -> V2 (-1)   0
                   'R' -> V2 1    0

manhattanDist :: V2 Int -> Int
manhattanDist (V2 x y) = abs x + abs y

part1 :: IO ()
part1 = do
  wireStrs <- fmap (splitOn ",") . lines
           -- <$> readFile "/Users/santi/playground/AOC/src/day3/input.txt"
           <$> readFile "/Users/santi/playground/AOC/src/day3/test1.txt"
           -- <$> readFile "/Users/santi/playground/AOC/src/day3/test2.txt"
  let steps = fmap (concatMap parseSteps) wireStrs
  let points = fmap (scanl1 (+)) steps
  let intersects = foldr1 S.intersection . fmap S.fromList $ points
  let result = minWith manhattanDist . S.toList $ intersects
  print . manhattanDist $ result

part2 :: IO ()
part2 = do
  wireStrs <- fmap (splitOn ",") . lines
           <$> readFile "/Users/santi/playground/AOC/src/day3/input.txt"
           -- <$> readFile "/Users/santi/playground/AOC/src/day3/test1.txt"
           -- <$> readFile "/Users/santi/playground/AOC/src/day3/test2.txt"
  let steps = fmap (concatMap parseSteps) wireStrs
  let points = flip zip [1..] . scanl1 (+) <$> steps
  let intersects = foldr1 (M.intersectionWith (+)) . fmap M.fromList $ points
  let result = minimum . M.elems $ intersects
  print result

main :: IO ()
main = part1 >> part2
