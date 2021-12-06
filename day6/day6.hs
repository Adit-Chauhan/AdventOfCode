{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.List
import qualified Data.Map as M

main = do
  inp <- readFile "input"
  let fishes = read ("[" ++ head (lines inp) ++ "]") :: [Int]
      headliner = 0 : map (toInteger . length) (group (sort fishes)) ++ [0]
      laggers = replicate 9 (0 :: Integer)
      sumTpl (x, y) = sum x + sum y
  putStr "Part One => "
  print $ sumTpl $ calculate 80 (headliner, laggers)
  putStr "Part Two => "
  print $ sumTpl $ calculate 256 (headliner, laggers)

calculate days lsts = foldl update' lsts [0 .. (days -1)]
  where
    update' :: ([Integer], [Integer]) -> Int -> ([Integer], [Integer])
    updateElem :: [Integer] -> Integer -> Int -> [Integer]
    updateLag :: [Integer] -> Int -> [Integer] -> ([Integer], [Integer])
    idx :: Int -> Int

    update' (headliners, lag : laggers) x = updateLag laggers x $ updateElem headliners lag x
    update' _ _ = error "Lsp Shut up"
    updateElem bulk ll x = case splitAt (idx x) bulk of
      (before, elem' : after) -> before ++ [elem' + ll] ++ after
      _ -> bulk
    updateLag lags x headline = (headline, lags ++ [headline !! idx x])
    idx x = x `rem` 7

-- Naive Implementaion of Algo
-- Slow Wanted to try newType and inctancing
newtype Fish = Fish Int
  deriving stock (Show, Eq)
  deriving newtype (Read)

instance Enum Fish where
  pred (Fish 0) = Fish 6
  pred (Fish x) = Fish (x - 1)

partOneNaive day = do
  inp <- readFile "input"
  let fishes = read ("[" ++ head (lines inp) ++ "]") :: [Fish]
  print $ map (partOne fishes) [0 .. day]

partOne :: [Fish] -> Int -> Int
partOne xs day = length $ iterate reduce' xs !! day
  where
    reduce' xs = map pred xs ++ replicate (countZero xs) (Fish 8)
    countZero xs = length $ filter (== Fish 0) xs
