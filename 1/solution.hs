isIncrease :: Int -> Int -> Bool
isIncrease x y = x < y

pairs :: [Int] -> [(Int, Int)]
pairs (a : b : as) = (a, b) : pairs (b : as)
pairs _ = []

slidingWindow :: [Int] -> [Int]
slidingWindow (a : b : c : rest) = a + b + c : slidingWindow (b : c : rest)
slidingWindow _ = []

totalIncreases :: [Int] -> Int
totalIncreases = length . filter (uncurry isIncrease) . pairs

windowIncreases :: [Int] -> Int
windowIncreases = length . filter (uncurry isIncrease) . pairs . slidingWindow

-- reading / testing

readNumbers :: String -> IO [Int]
readNumbers fileName = map read . lines <$> readFile fileName

testNumbers = [199,200,208,210,200,207,240,269,260,263]

testAnswer = 7 == totalIncreases testNumbers
