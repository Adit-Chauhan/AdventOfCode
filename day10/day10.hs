import Data.List (sort)

main = do
  inp <- readFile "input"
  print $ partOne (lines inp)
  print $ partTwo (lines inp)

expect '[' = ']'
expect '{' = '}'
expect '<' = '>'
expect '(' = ')'

isOpenBracket '[' = True
isOpenBracket '{' = True
isOpenBracket '<' = True
isOpenBracket '(' = True
isOpenBracket _ = False

partOne :: [String] -> Int
partOne lst = foldl (\score (elems, corrupt) -> if corrupt then trace ("elems " ++ elems) findScore (head elems) + score else score) 0 (map checker lst)
  where
    findScore ')' = 3
    findScore ']' = 57
    findScore '}' = 1197
    findScore '>' = 25137
    findScore x = 0

partTwo lst = sLst !! (length sLst `div` 2) -- `debug` (show sLst ++ "length sLst + 1" ++ show (length sLst `div` 2))
  where
    sLst = sort $ filter (/= (-1)) $ map ((`div` 5) . partTT . checker) lst
    partTT (es, b) =
      let scoreCalc = foldl autoCompleteScore 0
       in if b
            then (-1)
            else scoreCalc es
    autoCompleteScore score elem' = (5 * score) + aScore elem'
    aScore ')' = 1
    aScore ']' = 2
    aScore '}' = 3
    aScore '>' = 4
    aScore _ = 0

checker :: [Char] -> ([Char], Bool)
checker = foldl checkerFoldFunc (['|'], False)
  where
    checkerFoldFunc (x : expected, done) elem
      | done = (x : expected, done) -- `debug` ("In done x = " ++ show x ++ "\n")
      | x == elem = (expected, False) -- `debug` ("Found Element new Expected " ++ show expected ++ "\n")
      | isOpenBracket elem = (expect elem : x : expected, False) -- `debug` ("Open bracket => " ++ show elem ++ "\n")
      | not (isOpenBracket elem) = ([elem], True) -- `debug` "Currupt list"
      | otherwise = (x : expected, False) -- `debug` "Incomplete list"
