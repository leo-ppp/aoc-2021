-- |

module Solution where

data Move = Forward Int | Up Int | Down Int deriving Show

doMove :: Move -> (Int, Int) -> (Int, Int)
doMove (Forward a) (pos, dep) = (pos + a, dep)
doMove (Up a) (pos, dep) =      (pos, dep - a)
doMove (Down a) (pos, dep) =    (pos, dep + a)

travel :: [Move] -> (Int, Int)
travel = foldr doMove (0, 0)

distance :: (Int, Int) -> Int
distance = uncurry (*)

-----

doMoveAim :: Move -> (Int, Int, Int) -> (Int, Int, Int)
doMoveAim (Forward x) (pos, dep, aim) = (pos + x, dep + (aim * x), aim)
doMoveAim (Up x) (pos, dep, aim) =      (pos, dep, aim - x)
doMoveAim (Down x) (pos, dep, aim) =    (pos, dep, aim + x)

travelWithAim :: [Move] -> (Int, Int)
travelWithAim moves = let (pos, dep, aim) = foldl (flip doMoveAim) (0, 0, 0) moves
                      in (pos, dep)

-----

readMoves :: String -> IO [Move]
readMoves fileName = map readMove . lines <$> readFile fileName

solvept1 :: String -> IO Int
solvept1 fileName = distance . travel <$> readMoves fileName

solvept2 :: String -> IO Int
solvept2 fileName = distance . travelWithAim <$> readMoves fileName

matchMoveType :: String -> (Int -> Move)
matchMoveType "forward" = Forward
matchMoveType "up" = Up
matchMoveType "down" = Down
matchMoveType _ = error "Unknown move type"

readMove :: String -> Move
readMove str = let [ty, dist] = words str
                   in matchMoveType ty (read dist)
