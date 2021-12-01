


readNumbers :: String -> IO [Integer]
readNumbers fileName = map read . lines <$> readFile fileName
