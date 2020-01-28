import Data.Digits
import Data.List.HT hiding (span)

import Control.Monad

-- main :: IO ()
main = do
  let (lower, upper) = (347312, 805915)
  print . length . filter (pred1 . digits 10) $ [lower..upper]
  print . length . filter (pred2 . digits 10) $ [lower..upper]
  where
    pred1 = (&&) <$> part1Adj <*> isAscending
    pred2 = (&&) <$> part2Adj <*> isAscending
    part1Adj  = or . mapAdjacent (==)
    part2Adj [] = False
    part2Adj (x:xs) = let (equalsX, rest) = span (x ==) xs
                      in length equalsX == 1 || part2Adj rest

