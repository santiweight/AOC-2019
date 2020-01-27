main = do
  path <- readFile "/Users/santi/playground/AOC/day1/input.txt"
  let nums = read <$> lines path
  return $ (,)
    (sum $ fmap calcFuel nums)
    (sum $ fmap calcFixedFuel nums)

calcFuel :: Int -> Int
calcFuel = subtract 2 . flip div 3

calcFixedFuel mass = let fuel = calcFuel mass
                     in if fuel > 0
                        then fuel + calcFixedFuel fuel
                        else 0
