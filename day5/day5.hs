import qualified Data.Map as Map

main = do
  inp <- readFile "input"
  let partOneMap = partOne . partOneFilter . parseInput $ lines inp
      partTwoMap = partTwo partOneMap (partTwoFilter . parseInput $ lines inp)
  putStr "Part One = "
  print $ length $ Map.filter (> 1) partOneMap
  putStr "Part Two = "
  print $ length $ Map.filter (> 1) partTwoMap

parseInput :: [String] -> [[(Int, Int)]]
parseInput = map (toIntlst . words)
  where
    toIntlst x = case x of
      [from, _, to] -> [readTup from, readTup to]
      _ -> error "Error in Input"
    readTup x = read ("(" ++ x ++ ")") :: (Int, Int)

partOneFilter :: [[(Int, Int)]] -> [[(Int, Int)]]
partOneFilter = filter (\a -> fst (a !! 1) == fst (a !! 0) || snd (a !! 1) == snd (a !! 0))

partTwoFilter :: [[(Int, Int)]] -> [[(Int, Int)]]
partTwoFilter = filter $ not . (\a -> fst (a !! 1) == fst (a !! 0) || snd (a !! 1) == snd (a !! 0))

partOne :: [[(Int, Int)]] -> Map.Map (Int, Int) Int
partOne = foldl insertPoints Map.empty
  where
    insertPoints acc coords = foldl (\acu cord -> Map.insertWith (+) cord 1 acu) acc (getPoints coords)
    getPoints :: [(Int, Int)] -> [(Int, Int)]
    getPoints [(x1, y1), (x2, y2)]
      | x1 == x2 = zip (repeat x1) ([y1 .. y2] ++ [y2 .. y1])
      | y1 == y2 = zip ([x1 .. x2] ++ [x2 .. x1]) $ repeat y1

partTwo :: Map.Map (Int, Int) Int -> [[(Int, Int)]] -> Map.Map (Int, Int) Int
partTwo = foldl insertPoints
  where
    insertPoints acc coords = foldl (\acu cord -> Map.insertWith (+) cord 1 acu) acc (getPoints coords)
    getPoints :: [(Int, Int)] -> [(Int, Int)]
    getPoints [(x1, y1), (x2, y2)]
      | x1 > x2 && y1 > y2 = zip [x1, (x1 - 1) .. x2] [y1, (y1 - 1) .. y2]
      | x1 > x2 && y1 < y2 = zip [x1, (x1 - 1) .. x2] [y1 .. y2]
      | x1 < x2 && y1 > y2 = zip [x1 .. x2] [y1, (y1 - 1) .. y2]
      | x1 < x2 && y1 < y2 = zip [x1 .. x2] [y1 .. y2]
