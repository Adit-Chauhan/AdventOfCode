import Data.Char

main = do
  inp <- readFile "./input_q1"
  print $ dount . windows $ (map (read :: String -> Int) $ lines inp)

dount :: (Ord a, Num b) => [a] -> b
dount [] = 0
dount [_] = 0
dount (x1 : x2 : xs) = if x1 < x2 then 1 + dount (x2 : xs) else dount (x2 : xs)

windows :: [Int] -> [Int]
windows [_] = []
windows xs = (sum $ take 3 xs) : (windows $ tail xs)
