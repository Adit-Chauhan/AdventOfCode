#!/bin/sh

mkdir day$1
cd day$1
wget --no-cookies --header "Cookie: session=53616c7465645f5f527f162a4e86f07b6d52d5f83d46542442d318678fc3d095b7151b7c9c69647b3c583a37913e48b1" https://adventofcode.com/2021/day/$1/input

echo '
main = do
  inp <- readFile "input"
  print $ lines inp
' >> day$1.hs

runhaskell day$1.hs
