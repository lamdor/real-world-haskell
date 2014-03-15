import Data.Char
import Data.List

-- exercise 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:xs) = if (null xs)
                  then Just x
                  else safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- exercise 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs =
  let (a,b) = break f xs in
  if null b
  then [a]
  else a : splitWith f (tail b)

-- exercise 3
firstWordOfEachLine :: String -> [String]
firstWordOfEachLine  =
  map firstWord . lines
  where firstWord = takeWhile (not . isSeparator)

printFirstWord :: String -> IO ()
printFirstWord = putStr . unlines . firstWordOfEachLine

-- exercise 4 (sorta cheated by using Data.List.transpose)
transposeText :: String -> String
transposeText = unlines . transpose . lines


-- exercises pg 97

-- exercise 1,2,3
asInt_fold :: String -> Int
asInt_fold ('-':xs) = (-1) * asInt_fold xs
asInt_fold []       = 0
asInt_fold xs       = foldl step 0 xs
  where step acc c = acc * 10 + digitToInt c

-- exercise 4 (so i know some stuff...)
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':xs) = fmap negate $ asInt_either xs
asInt_either []       = Right 0
asInt_either xs       = foldl step (Right 0) xs
  where step (Right acc) c = if isNumber c
                             then Right $ acc * 10 + digitToInt c
                             else Left ("non-digit '" ++ [c] ++ "'")
        step l@(Left _) _  = l

-- exercises 5,6
concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss

-- exercise 7
takeWhile_recursive :: (a -> Bool) -> [a] -> [a]
takeWhile_recursive p [] = []
takeWhile_recursive p (x:xs) | p x = x : takeWhile_recursive p xs
                             | otherwise = []

takeWhile_fold :: (a -> Bool) -> [a] -> [a]
takeWhile_fold p xs = foldr step [] xs
  where step x xs | p x = x : xs
                  | otherwise = []

-- exercise 8,9
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' eq xs =
  let (acc, res) = foldr step ([], []) xs in
  if (null acc) then res else acc : res
  where step x ([], res) = ([x], res)
        step x ((y:ys), res) | eq x y = (x:y:ys, res)
                             | otherwise = ([x], (y:ys) : res)

-- exercise 10
any_fold :: (a -> Bool) -> [a] -> Bool
any_fold p xs = foldl' step False xs
  where step True _ = True
        step _ x | p x = True
                 | otherwise = False

cycle_fold :: [a] -> [a]
cycle_fold xs = loop xs xs
  where loop [] orig = loop orig orig
        loop (x:xs) orig = x : loop xs orig

words_fold :: String -> [String]
words_fold str =
  let (res, left) = foldr step ([], "") str in
  if (null left) then res else left : res
  where step ' ' (xs, str) | null str = (xs, "")
                           | otherwise = (str : xs, "")
        step c (xs, str)   = (xs, c : str)

unlines_fold :: [String] -> String
unlines_fold xs = foldr step "" xs
  where step str res = str ++ "\n" ++ res
