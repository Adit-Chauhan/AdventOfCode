{-# LANGUAGE FlexibleContexts #-}

import Control.Parallel.Strategies
import Data.List

main = do
  inp <- readFile "input"
  let crabs = read ("[" ++ head (lines inp) ++ "]") :: [Int]
      fuelCrab = fuelAt crabs
      fuelApCrab = apFuelAt crabs
      results = parMap rdeepseq fuelCrab [0 .. 1855] :: [Int]
      results' = parMap rdeepseq fuelApCrab [0 .. 1855] :: [Int]
  putStr "Part One => "
  print $ minimum results
  putStr "Part Two => "
  print $ minimum results'

fuelAt :: [Int] -> Int -> Int
fuelAt crabs position = sum $ map (abs . (position -)) crabs

apSum' :: Int -> Int
apSum' n = n * (2 + (n - 1)) `div` 2

apFuelAt :: [Int] -> Int -> Int
apFuelAt crabs position = sum $ map apSum' $ map (abs . (position -)) crabs
