-- |

module Day4 where
import Data.List ( transpose )

type Space = (Int, Bool)
type Board = [[Space]]

unmarked :: Int -> Space
unmarked x = (x, False)

mark :: Space -> Space
mark (x, _) = (x, True)

isMarked :: Space -> Bool
isMarked (_, b) = b

isUnmarked :: Space -> Bool
isUnmarked = not . isMarked

spaceNumber :: Space -> Int
spaceNumber (x, _) = x

markIfMatching :: Int -> Space -> Space
markIfMatching x sp = if x == spaceNumber sp then mark sp else sp

isWinning :: Board -> Bool
isWinning b = any winningLine b || any winningLine (transpose b)
  where winningLine = all isMarked

updateBoards :: Int -> [Board] -> [Board]
updateBoards move = map (updateMove move)
  where updateMove :: Int -> Board -> Board
        updateMove x board = map (map (markIfMatching x)) board

-- returns the winning move and board.
step :: [Int] -> [Board] -> (Int, Board)
step [] _ = error "No more moves left without a winner"
step (move : moves) boards = if not (null winners)
                               then (move, head winners)
                               else step moves updatedBoards
  where updatedBoards :: [Board]
        updatedBoards = updateBoards move boards
        winners :: [Board]
        winners = filter isWinning updatedBoards

computeScore :: (Int, Board) -> Int
computeScore (move, board) = move * sum (unmarkedSpaces board)
  where unmarkedSpaces :: Board -> [Int]
        unmarkedSpaces board = map spaceNumber . concatMap (filter isUnmarked) $ board

solvePt1 :: ([Int], [Board]) -> Int
solvePt1 (moves, boards) = computeScore $ step moves boards

-------- pt2

stepLoss :: [Int] -> [Board] -> (Int, Board)
stepLoss [] _ = error "No more moves left without a loser"
stepLoss (move : moves) boards = if null losers
                                 then (move, head winners)
                                 else stepLoss moves losers
  where updatedBoards :: [Board]
        updatedBoards = updateBoards move boards
        winners :: [Board]
        winners = filter isWinning updatedBoards
        losers :: [Board]
        losers = filter (not . isWinning) updatedBoards

solvePt2 :: ([Int], [Board]) -> Int
solvePt2 (moves, boards) = computeScore $ stepLoss moves boards

-------- parsing

splitOnCommas :: String -> [String]
splitOnCommas s = case dropWhile (== ',') s of
                      "" -> []
                      s' -> w : splitOnCommas s''
                            where (w, s'') = break (== ',') s'

readBoardLines :: [String] -> [Board]
readBoardLines [] = []
readBoardLines boards = (map readBoardLine . take 5 $ boards) : readBoardLines (drop 5 boards)
  where readBoardLine :: String -> [Space]
        readBoardLine = map (unmarked . read) . words

parseLines :: [String] -> ([Int], [Board])
parseLines [] = error "Empty file given to parseLines"
parseLines (nums : boards) = (readNums, readBoardLines boards)
  where readNums = (map read . splitOnCommas) nums

parse :: String -> ([Int], [Board])
parse = parseLines . filter (/= "") . lines

parseFromFile :: String -> IO ([Int], [Board])
parseFromFile path = parse <$> readFile path

testMoves :: [Int]
testMoves = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]

testBoards :: [Board]
testBoards = [[[(22,False),(13,False),(17,False),(11,False),(0,False)]
         ,[(8,False),(2,False),(23,False),(4,False),(24,False)]
         ,[(21,False),(9,False),(14,False),(16,False),(7,False)]
         ,[(6,False),(10,False),(3,False),(18,False),(5,False)]
         ,[(1,False),(12,False),(20,False),(15,False),(19,False)]],
         [[(3,False),(15,False),(0,False),(2,False),(22,False)],
          [(9,False),(18,False),(13,False),(17,False),(5,False)],
          [(19,False),(8,False),(7,False),(25,False),(23,False)],
          [(20,False),(11,False),(10,False),(24,False),(4,False)],
          [(14,False),(21,False),(16,False),(12,False),(6,False)]]
        ,[[(14,False),(21,False),(17,False),(24,False),(4,False)]
         ,[(10,False),(16,False),(15,False),(9,False),(19,False)],
          [(18,False),(8,False),(23,False),(26,False),(20,False)]
         ,[(22,False),(11,False),(13,False),(6,False),(5,False)]
         ,[(2,False),(0,False),(12,False),(3,False),(7,False)]]]

