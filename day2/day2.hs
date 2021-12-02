import Text.XHtml (input)

data Moment = Forward Int | Down Int | Up Int deriving (Show)

moment :: [String] -> [Int] -> [Int]
moment x y
  | head x == "forward" = [head y + k, y !! 1]
  | head x == "down" = [head y, y !! 1 + k]
  | head x == "up" = [head y, y !! 1 - k]
  where
    k = read (x !! 1) :: Int

main' = do
  inp <- readFile "./input"
  print $ product $ foldr (moment . words) [0, 0] $ lines inp

moment' :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
moment' x y
  | head y == "forward" = (fst x + k, snd x + (thrd x * k), thrd x)
  | head y == "down" = (fst x, snd x, thrd x + k)
  | head y == "up" = (fst x, snd x, thrd x - k)
  where
    k = read (y !! 1) :: Int
    fst (x, _, _) = x
    snd (_, x, _) = x
    thrd (_, _, x) = x

result x = fst x * snd x
  where
    fst (x, _, _) = x
    snd (_, x, _) = x

main = do
  inp <- readFile "./input"
  print $ foldl moment' (0, 0, 0) (map words $ lines inp)
