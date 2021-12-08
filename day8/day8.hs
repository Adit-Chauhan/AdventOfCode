{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

type SetC = S.Set Char

type MIntS = M.Map Int (S.Set Char)

type MSetC = M.Map Char (S.Set Char)

main = do
  inp <- readFile "input"
  let observations = map (\x -> splitOn "|" x !! 0) $ lines inp
      readings = map (\x -> splitOn "|" x !! 1) $ lines inp
      decodes = map (decode . words) observations
      pairings = zip decodes readings
      part2 (dec, red) = getNumber dec red
  print $ sum (map part2 pairings)

decode obs =
  let nm = numMap obs
      c5 = common 5 obs
      u5 = uncommon 5 obs
      c6 = common 6 obs
      u6 = uncommon 6 obs
      bindings = get'f nm $ get'c u6 $ get'e u5 nm $ get'b nm $ get'd c5 $ get'g c5 c6 $ get'a nm
   in reMap bindings

common n lst = common' $ filter (\x -> length x == n) lst
  where
    common' :: [String] -> SetC
    common' = foldl (\acc x -> S.intersection acc $ S.fromList x) (S.fromList "abcdefg")

uncommon :: Int -> [String] -> SetC
uncommon n lst = S.difference (S.fromList "abcdefg") $ common n lst

numMap :: [String] -> MIntS
numMap = foldl mapAssigner M.empty
  where
    mapAssigner base x = case length x of
      2 -> M.insert 1 (S.fromList x) base
      3 -> M.insert 7 (S.fromList x) base
      4 -> M.insert 4 (S.fromList x) base
      7 -> M.insert 8 (S.fromList x) base
      _ -> base

get'a :: MIntS -> MSetC
get'a nm =
  toMap M.empty 'a' $ diff (M.lookup 7 nm) (M.lookup 1 nm)
  where
    diff (Just x) (Just y) = S.difference x y

get'g :: SetC -> SetC -> MSetC -> MSetC
get'g com5 com6 segMap = toMap segMap 'g' $ diff (M.lookup 'a' segMap)
  where
    diff (Just a) = S.difference (S.intersection com5 com6) a

get'd :: SetC -> MSetC -> MSetC
get'd com5 segMap = toMap segMap 'd' $ diff com5 (M.lookup 'a' segMap) (M.lookup 'g' segMap)
  where
    diff com (Just a) (Just g) = S.difference (S.difference com a) g

get'b :: MIntS -> MSetC -> MSetC
get'b nm segMap = toMap segMap 'b' $ diff (M.lookup 4 nm) (M.lookup 1 nm) (M.lookup 'd' segMap)
  where
    diff (Just four) (Just one) (Just d) = S.difference (S.difference four one) d

get'e :: SetC -> MIntS -> MSetC -> MSetC
get'e ucom5 nm segMap = toMap segMap 'e' $ diff ucom5 (M.lookup 1 nm) (M.lookup 'b' segMap)
  where
    diff ucom (Just one) (Just b) = S.difference (S.difference ucom one) b

get'c :: SetC -> MSetC -> MSetC
get'c ucom6 segMap = toMap segMap 'c' $ diff ucom6 (M.lookup 'd' segMap) (M.lookup 'e' segMap)
  where
    diff ucom (Just d) (Just e) = S.difference (S.difference ucom d) e

get'f :: MIntS -> MSetC -> MSetC
get'f nm segMap = toMap segMap 'f' $ diff (M.lookup 1 nm) (M.lookup 'c' segMap)
  where
    diff (Just one) (Just f) = S.difference one f

reMap :: MSetC -> M.Map SetC Char
reMap segMap = M.fromList $ zip (map newCode ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]) ['0' .. '9']
  where
    newCode s = foldl (\acc x -> S.union acc (unwrap $ M.lookup x segMap)) S.empty s
    unwrap (Just a) = a

getNumber newMap s = read (map (\w -> fromJust (M.lookup (S.fromList w) newMap)) $ words s) :: Int

toMap m c i = M.insert c i m
