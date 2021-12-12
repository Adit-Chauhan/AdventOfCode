import Data.Char (digitToInt)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

debug = flip trace

type Grid = M.Map (Int, Int) Int

type Sii = S.Set (Int, Int)

main = do
  inp <- makeGrid 9 9 . concatMap (map digitToInt) . lines <$> readFile "input"
  putStrLn $ stringGrid inp
  print (foldl partOne (inp, 0) [1 .. 100])
  print $length $ takeWhile snd $ scanl partTwo (inp, True) [1 ..]

partOne :: (Grid, Int) -> Int -> (Grid, Int)
partOne (grid, ints) _ = (nG, sf)
  where
    nG = fst $ update grid
    sf = ints + snd (update grid)
    fst (x, _, _) = x
    snd (_, x, _) = x

partTwo (grid, sync) _ = (nG, sy)
  where
    nG = fst $ update grid
    sy = thrd $ update grid
    fst (x, _, _) = x
    thrd (_, _, x) = x

makeGrid :: Int -> Int -> [Int] -> Grid
makeGrid row col lst = M.fromList $ zip [(x, y) | x <- [0 .. row], y <- [0 .. col]] lst

stringGrid = M.foldlWithKey (\acc k v -> if snd k == 9 then acc ++ show v ++ "\n" else acc ++ show v ++ " ") ""

updateNeighbours g (r, c) = foldl (flip (M.adjust succ)) g [(r + 1, c), (r -1, c), (r, c + 1), (r, c -1), (r -1, c -1), (r + 1, c + 1), (r -1, c + 1), (r + 1, c -1), (r, c)]

update :: Grid -> (Grid, Int, Bool)
update grid
  | isStable incGrid = (incGrid, 0, True) `debug` ("update Result \n" ++ stringGrid incGrid)
  | otherwise = update' incGrid (flashers incGrid) S.empty `debug` ("To Stabalize \n" ++ stringGrid incGrid)
  where
    incGrid = M.map succ grid
    isStable gr = null $ M.filter (> 9) gr
    flashers gr = S.fromList . M.keys $ M.filter (> 9) gr `debug` ("flashers  " ++ show (S.fromList . M.keys $ M.filter (> 9) gr))

update' :: Grid -> Sii -> Sii -> (Grid, Int, Bool)
update' gr flashers flashed
  | isStable' flashers allFlash = (M.map (\v -> if v > 9 then 0 else v) emulateFlash, S.size allFlash, S.size allFlash /= M.size emulateFlash)
  | otherwise = update' emulateFlash allFlash flashers `debug` ("Intermediate \n" ++ stringGrid emulateFlash)
  where
    emulateFlash = S.foldl updateNeighbours gr $ S.difference flashers flashed `debug` ("To flash  " ++ show (S.difference flashers flashed))
    allFlash = S.fromList . M.keys $ M.filter (> 9) emulateFlash
    isStable' flashed allFlashers = S.difference allFlashers flashed == S.empty
