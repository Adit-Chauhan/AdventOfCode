{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace

main = do
  inp <- readFile "input"
  let observations = map (\x -> splitOn "|" x !! 0) $ lines inp
      readings = map (\x -> splitOn "|" x !! 1) $ lines inp
      decodes = map (decode . words) observations
      pairings = zip decodes readings
      part2 (dec, red) = getNumber dec red
  print $ zip observations readings
  print $ sum (map part2 pairings)

decode obs =
  let nm = numMap obs
      c5 = common5 obs
      u5 = uncommon5 obs
      c6 = common6 obs
      u6 = uncommon6 obs
      bindings = get'f nm $ get'c u6 $ get'e u5 nm $ get'b nm $ get'd c5 $ get'g c5 c6 $ get'a nm
   in reMap bindings

common :: [String] -> S.Set Char
common = foldl (\acc x -> S.intersection acc $ S.fromList x) (S.fromList "abcdefg")

uncommon :: [String] -> S.Set Char
uncommon lst = S.difference (S.fromList "abcdefg") $ common lst

uncommon5 lst = S.difference (S.fromList "abcdefg") $ common5 lst

uncommon6 lst = S.difference (S.fromList "abcdefg") $ common6 lst

common5 lst = common $ filter (\x -> length x == 5) lst

common6 lst = common $ filter (\x -> length x == 6) lst

numMap :: [String] -> M.Map Int (S.Set Char)
numMap = foldl mapAssigner M.empty
  where
    mapAssigner base x = case length x of
      2 -> M.insert 1 (S.fromList x) base
      3 -> M.insert 7 (S.fromList x) base
      4 -> M.insert 4 (S.fromList x) base
      7 -> M.insert 8 (S.fromList x) base
      _ -> base

get'a :: M.Map Int (S.Set Char) -> M.Map Char (S.Set Char)
get'a nm =
  --(trace ("\nnmMap " ++ show nm ++ "\nget'a => 7 : " ++ show (M.lookup 7 nm) ++ " | 1 : " ++ show (M.lookup 1 nm))) $
  toMap M.empty 'a' $ diff (M.lookup 7 nm) (M.lookup 1 nm)
  where
    diff (Just x) (Just y) = S.difference x y
    diff _ _ = error "get'a None Found"

get'g :: S.Set Char -> S.Set Char -> M.Map Char (S.Set Char) -> M.Map Char (S.Set Char)
get'g com5 com6 segMap = toMap segMap 'g' $ diff (M.lookup 'a' segMap)
  where
    diff (Just a) = S.difference (S.intersection com5 com6) a
    diff _ = error "get'g None Found"

get'd com5 segMap = toMap segMap 'd' $ diff com5 (M.lookup 'a' segMap) (M.lookup 'g' segMap)
  where
    diff com (Just a) (Just g) = S.difference (S.difference com a) g
    diff _ _ _ = error "get'd None Found"

get'b nm segMap = toMap segMap 'b' $ diff (M.lookup 4 nm) (M.lookup 1 nm) (M.lookup 'd' segMap)
  where
    diff (Just four) (Just one) (Just d) = S.difference (S.difference four one) d
    diff _ _ _ = error "get'b None Found"

get'e :: S.Set Char -> M.Map Int (S.Set Char) -> M.Map Char (S.Set Char) -> M.Map Char (S.Set Char)
get'e ucom5 nm segMap = toMap segMap 'e' $ diff ucom5 (M.lookup 1 nm) (M.lookup 'b' segMap)
  where
    diff :: S.Set Char -> Maybe (S.Set Char) -> Maybe (S.Set Char) -> S.Set Char
    diff ucom (Just one) (Just b) = S.difference (S.difference ucom one) b
    diff _ _ _ = error "get'e None Found"

get'c ucom6 segMap = toMap segMap 'c' $ diff ucom6 (M.lookup 'd' segMap) (M.lookup 'e' segMap)
  where
    diff ucom (Just d) (Just e) = S.difference (S.difference ucom d) e
    diff _ _ _ = error "get'c None Found"

get'f nm segMap = toMap segMap 'f' $ diff (M.lookup 1 nm) (M.lookup 'c' segMap)
  where
    diff (Just one) (Just f) = S.difference one f
    diff _ _ = error "get'f None Found"

reMap segMap = M.fromList $ zip (map newCode ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]) ['0' .. '9']
  where
    newCode s = foldl (\acc x -> S.union acc (unwrap $ M.lookup x segMap)) S.empty s
    unwrap (Just a) = a

getNumber newMap s = read (map (\w -> fromJust (M.lookup (S.fromList w) newMap)) $ words s) :: Int

toMap m c i = M.insert c i m
