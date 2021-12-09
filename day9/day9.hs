import Data.Char (digitToInt)
import Data.Foldable
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S

type Grid = M.Map (Int, Int) Int

lastN' n xs = foldl' (const . drop 1) xs (drop n xs)

main = do
  inp <- readFile "input"
  let grid = makeGrid 99 99 $ concatMap (map digitToInt) $ lines inp
      lowPoints = M.foldlWithKey (\points k v -> if isLow k grid then k : points else points) [] grid
      fukenList = foldl (\(basinsList, global) x -> let thisBasin = basins (basinsList, global) [x] S.empty grid in ((thisBasin : basinsList), (S.union thisBasin global))) ([], S.empty) lowPoints
  putStr "Part One => "
  print $ M.foldlWithKey (\acc k v -> if isLow k grid then v + 1 + acc else acc) 0 grid
  putStr "Part Two => "
  print $ product . lastN' 3 $ sort (map S.size (fst fukenList))

isLow :: (Int, Int) -> Grid -> Bool
isLow (r, c) grid
  | search (r, c) == 9 = False
  | search (r, c) == minimum adjVals = True
  | otherwise = False
  where
    adjVals = [search (r + 1, c), search (r -1, c), search (r, c + 1), search (r, c -1), search (r, c)]
    search (x, y) = unwrap (M.lookup (x, y) grid)

makeGrid :: Int -> Int -> [Int] -> Grid
makeGrid row col lst = M.fromList $ zip [(x, y) | x <- [0 .. row], y <- [0 .. col]] lst

basins :: ([S.Set (Int, Int)], S.Set (Int, Int)) -> [(Int, Int)] -> S.Set (Int, Int) -> Grid -> S.Set (Int, Int)
basins _ [] localVisited _ = localVisited
basins (b, visited) (x : toSearch) localVisited grid =
  let neighbours (r, c) = foldl (\acc e -> if search e < 9 && not (S.member e vall) then e : acc else acc) toSearch [(r + 1, c), (r -1, c), (r, c + 1), (r, c -1)]
      v2 = S.insert x localVisited
      vall = S.union v2 visited
      search (x, y) = unwrap (M.lookup (x, y) grid)
   in if not (S.member x visited)
        then basins (b, visited) (neighbours x) v2 grid
        else localVisited

unwrap (Just x) = x
unwrap Nothing = 100
