foldl2 :: (a -> b -> a) -> a -> [b] -> a
foldl2 step zero (x:xs) = foldl step (step zero x) xs
foldl2 _ zero [] = zero

sum_foldl' xs = foldl' (+) 0 xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' step zero (x:xs) = step x (foldr' step zero xs)
foldr' _ zero [] = zero

sum_foldr' xs = foldr' (+) 0 xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr' step [] xs
  where step x ys | p x       = x : ys
                  | otherwise = ys

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr' step [] xs
  where step x ys = f x : ys

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
  where step x g a = g (f a x)

identity :: [a] -> [a]
identity xs = foldr (:) [] xs

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- strict left fold
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ zero [] = zero
foldl' step zero (x:xs) =
  let new = step zero x
  in new `seq` foldl' step new xs
