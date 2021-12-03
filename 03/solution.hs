{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- |

module Day3 where
import Distribution.Simple.Compiler (CompilerFlavor(HaskellSuite))
import Data.List (transpose)

data BinNum = Zero | One deriving (Eq, Ord, Show)

type Bin = [BinNum]

mostCommon :: Bin -> BinNum
mostCommon bins = let ones = (length . filter (== One)) bins
                      zeros = (length . filter (== Zero)) bins
                in if zeros > ones then Zero else One

leastCommon :: Bin -> BinNum
leastCommon = flipBin . mostCommon
  where flipBin One = Zero
        flipBin Zero = One

gammaRate :: [Bin] -> Bin
gammaRate = map mostCommon . transpose

epsilonRate :: [Bin] -> Bin
epsilonRate = map leastCommon . transpose

binToInt :: Bin -> Int
binToInt bin = foldl incbin 0 indexedBin
  where
    indexedBin = zip (reverse bin) [0..]

    incbin :: Int -> (BinNum, Integer) -> Int
    incbin acc (Zero, _ix) = acc
    incbin acc (One, ix) = acc + (2 ^ ix)

solvePt1 :: [Bin] -> Int
solvePt1 bins = let gammaDec = binToInt (gammaRate bins)
                    epsDec = binToInt (epsilonRate bins)
                    in gammaDec * epsDec

---- p2

getAtPos :: [Bin] -> Int -> Bin
getAtPos bins ix = map (!! ix) bins

scrubber :: [Bin] -> Int -> (Bin -> BinNum) -> Bin
scrubber [bin] _ix common = bin
scrubber bins ix common = scrubber validBins (ix + 1) common
  where commonBin = common (getAtPos bins ix)
        validBins = filter (\b -> commonBin == (b !! ix)) bins

solvePt2 :: [Bin] -> Int
solvePt2 bins = let o2 = scrubber bins 0 mostCommon
                    co2 = scrubber bins 0 leastCommon
                    in binToInt o2 * binToInt co2

---- parsing

readBin :: String -> Bin
readBin = map readBinNum
  where
    readBinNum '1' = One
    readBinNum '0' = Zero

readFromFile :: String -> IO [Bin]
readFromFile fileName = map readBin . lines <$> readFile fileName

tests = [[Zero,Zero,One,Zero,Zero],[One,One,One,One,Zero],[One,Zero,One,One,Zero],[One,Zero,One,One,One],[One,Zero,One,Zero,One],[Zero,One,One,One,One],[Zero,Zero,One,One,One],[One,One,One,Zero,Zero],[One,Zero,Zero,Zero,Zero],[One,One,Zero,Zero,One],[Zero,Zero,Zero,One,Zero],[Zero,One,Zero,One,Zero]]
