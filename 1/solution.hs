isIncrease :: Ord a => a -> a -> Bool
isIncrease x y = x < y

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (a : b : as) = (a, b) : pairs (b : as)

totalIncreases :: Ord a => [a] -> Int
totalIncreases = length . filter (uncurry isIncrease) . pairs

-- reading / testing

readNumbers :: String -> IO [Int]
readNumbers fileName = map read . lines <$> readFile fileName

testNumbers = [199,200,208,210,200,207,240,269,260,263]

testAnswer = 7 == totalIncreases testNumbers
