import Data.Char

binToDec :: [Int] -> Int
binToDec x = sum $ zipWith ((*) . (2 ^)) [11, 10 .. 0] x

gamma :: [[Char]] -> [Int]
gamma x = map (fromEnum . (>= 500)) $ foldl ones (replicate 12 0) x
  where
    ones :: [Int] -> String -> [Int]
    ones acc x = zipWith ((+) . digitToInt) x acc

numberOfHeadOnes = foldl (\acc x -> acc + digitToInt (head x)) 0

oxygen :: [[Char]] -> [Int]
oxygen [x] = map digitToInt x
oxygen x =
  let common = fromEnum $ numberOfHeadOnes x >= ceiling (fromIntegral (length x) / 2)
      y = map tail $ filter (\s -> head s == chr (common + 48)) x
   in common : oxygen y

carbon :: [[Char]] -> [Int]
carbon [x] = map digitToInt x
carbon x =
  let common = fromEnum $ numberOfHeadOnes x < ceiling (fromIntegral (length x) / 2)
      y = map tail $ filter (\s -> head s == chr (common + 48)) x
   in common : carbon y

part_1 x = (4095 - g) * g
  where
    g = binToDec $ gamma x

part_2 x = binToDec (oxygen x) * binToDec (carbon x)

main = do
  inp <- readFile "input"
  putStr "Part One  "
  print $ part_1 (lines inp)
  putStr "Part Two  "
  print $ part_2 (lines inp)
