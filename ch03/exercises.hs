import Data.Foldable (Foldable, foldMap)
import Data.Monoid
import Data.List (sortBy)
import Data.Ord
  
-- exercise 1 & 2
sizeOf :: [a] -> Int
sizeOf [] = 0
sizeOf (x:xs) = 1 + sizeOf xs

-- exercise 3
mean :: Fractional a => [a] -> Maybe a
mean [] = Nothing
mean xs = Just (sumAll / len)
          where sumAll = sum xs
                len = fromIntegral (length xs)

-- exercise 3 using MONOIDS
mean' :: (Fractional a, Eq a, Foldable f) => f a -> Maybe a
mean' xs = uncurry divSums $ foldMap valueAndOneSum xs
           where valueAndOneSum x = (Sum x, Sum 1)
                 divSums (Sum a) (Sum 0) = Nothing
                 divSums (Sum a) (Sum b) = Just (a / b)

-- exercise 4
palindrome xs = xs ++ (reverse xs)

-- exercise 5
isPalindrome xs = palindrome xs == xs

-- exercise 6
sortByLength = sortBy (comparing length)

-- exercise 7
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (xs:[]) = xs
intersperse x (xs:xss) = xs ++ [x] ++ (intersperse x xss)

-- exercise 8
data Tree a = Node a (Tree a) (Tree a) | Empty
            deriving Show

treeHeight :: Tree a -> Int
treeHeight Empty          = 0
treeHeight (Node _ t1 t2) = 1 + ((treeHeight t1) `max` (treeHeight t2))

-- exercise 9
data Direction = LeftTurn
               | RightTurn
               | Straight
                 deriving (Show, Eq)

-- exercise 10
type Point = (Double, Double)

vector :: Point -> Point -> Point
vector (x1, y1) (x2, y2) = (x2-x1, y2-y1)

dot :: Point -> Point -> Double
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

norm :: Point -> Double
norm pt = sqrt (dot pt pt)

normalize :: Point -> Point
normalize (x,y) = (x / n, y / n)
                  where n = norm (x,y)

rot90CCW :: Point -> Point
rot90CCW (x,y) = (-y, x)

direction :: Point -> Point -> Point -> Direction
direction p1 p2 p3 =
  let v1 = vector p2 p1
      v2 = vector p2 p3
      v2rot = rot90CCW v2
  in
   case dot v1 v2rot of
     0.0       -> Straight
     x | x > 0 -> LeftTurn
     x | x < 0 -> RightTurn

-- exercise 11
directions :: [Point]  -> [Direction]
directions pts
  | length pts < 3 = []
  | otherwise = (direction p1 p2 p3) : directions (drop 1 pts)
                where [p1,p2,p3] = take 3 pts

-- exercise 12
polarAngleFrom :: Point => Point -> Double
polarAngleFrom pt1 pt2 =
  let (x,y) =  vector pt1 pt2
  in atan (y / x)

convexHull :: [Point]  -> [Point]
convexHull pts = restOrderedByPolarAngle
                 where
                   sortedByY = sortBy (comparing snd) pts
                   firstPoint = head $ sortedByY
                   restOrderedByPolarAngle =
                     sortBy (comparing (polarAngleFrom firstPoint)) $ tail sortedByY
  
points = [(0,0), (2,0), (2,2), (1,2), (0,2), (1,1)] :: [Point]

findNextPointFromAt :: [Point] -> Int -> Int -> Maybe Point
findNextPointFromAt pts fixed start =
  let pt1 = pts !! start
      pt2 = pts !! (start + 1)
      pt3 = pts !! fixed
  in if direction pt1 pt2 pt3 /= LeftTurn
     then if start > 1
          then findNextPointFromAt pts fixed (start - 1)
          else if fixed == length pts
               then findNextPointFromAt pts fixed (start + 1)
               else findNextPointFromAt pts (fixed + 1) (start + 1)
     else findNextPointFromAt pts (fixed + 1) (start + 1)

