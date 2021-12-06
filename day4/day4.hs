import Data.List (elemIndex, transpose)
import Data.Maybe (isJust)

main = do
  inp <- readFile "input"
  let boards = splitBoards $ drop 2 $ lines inp
      bingoQue = read ("[" ++ head (lines inp) ++ ",-100]") :: [Int]
      getWonBoard boards (Just val) = boards !! val
      --
      wonin = map (winTurns bingoQue) boards
      bestWon = sum $ map sum $ wonState bingoQue $ getWonBoard boards (elemIndex (minimum wonin) wonin)
      bestLoss = sum $ map sum $ wonState bingoQue $ getWonBoard boards (elemIndex (maximum wonin) wonin)
  print $ bingoQue !! (minimum wonin - 1) * bestWon
  print $ bingoQue !! (maximum wonin - 1) * bestLoss

wonState :: [Int] -> [[Int]] -> [[Int]]
wonState [] board = board
wonState (x : xs) board
  | isBoardWonWith 0 board = board
  | otherwise = wonState xs $ reduceBoardWith 0 board $ elementLocation x board

winTurns :: [Int] -> [[Int]] -> Int
winTurns [] _ = 100
winTurns (x : xs) board
  | isBoardWonWith (-5) board = 0
  | not (isBoardWonWith (-5) board) = 1 + winTurns xs (reduceBoardWith (-1) board $ elementLocation x board)

splitBoards :: [[Char]] -> [[[Int]]]
splitBoards [] = []
splitBoards x = map (map (read :: String -> Int) . words) (take 5 x) : splitBoards (drop 6 x)

elementLocation :: Int -> [[Int]] -> Maybe (Int, Int)
elementLocation element mat = unpack . head' . rowIdx $ colIdx
  where
    head' [] = (Nothing, 0)
    head' xs = head xs
    rowIdx = filter (isJust . fst)
    colIdx = zip (map (elemIndex element) mat) [0 ..]
    unpack a = case a of
      (Just x, y) -> Just (y, x)
      _ -> Nothing

isBoardWonWith :: Int -> [[Int]] -> Bool
isBoardWonWith ss' board = is_row_cut || is_col_cut
  where
    is_row_cut = ss' `elem` map sum board
    is_col_cut = ss' `elem` map sum (transpose board)

reduceBoardWith :: Int -> [[Int]] -> Maybe (Int, Int) -> [[Int]]
reduceBoardWith rep' board Nothing = board
reduceBoardWith rep' board (Just value) = replaceElem2D value rep' board
  where
    replaceElem2D :: (Int, Int) -> Int -> [[Int]] -> [[Int]]
    replaceElem2D (row, col) elem board = case splitAt row board of
      (before, k : after) -> before ++ replaceAtIndex col elem k : after
      _ -> board
    replaceAtIndex n item ls = a ++ (item : b) where (a, _ : b) = splitAt n ls
