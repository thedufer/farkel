import Data.Maybe
import Data.List

type Die = Int
type Score = Int
type Count = Int
data Result = Result Score Count deriving (Show, Eq)
data Strategy = Strategy [Score] [Count] deriving (Show)

score :: [Die] -> [Result]
score = removeDups . scoreWithDups

-- return highest score for each remaining die count
removeDups :: [Result] -> [Result]
removeDups = removeDupsSorted . (sortBy cmpR)

-- should only be called on sorted list
removeDupsSorted :: [Result] -> [Result]
removeDupsSorted [] = []
removeDupsSorted [x] = [x]
removeDupsSorted ((Result s1 c1):(Result s2 c2):rs)
  | c1 /= c2  = (Result s1 c1):(removeDupsSorted ((Result s2 c2):rs))
  | s1 < s2   = removeDupsSorted ((Result s2 c2):rs)
  | otherwise = removeDupsSorted ((Result s1 c1):rs)

cmpR :: Result -> Result -> Ordering
cmpR (Result _ c1) (Result _ c2)
  | c1 < c2  = GT
  | c1 > c2  = LT
  | c1 == c2 = EQ

-- scoreAll for all subsequences of [Die]
scoreWithDups :: [Die] -> [Result]
scoreWithDups xs =
  let subs = nonEmptySubsequences xs
  in nub $ catToResults (zip (map (((length xs) -) . length) subs) (map scoreAll subs))

catToResults :: [(Count, Maybe Score)] -> [Result]
catToResults [] = []
catToResults ((c, Just s):xs) = (Result c s):(catToResults xs)
catToResults ((c, Nothing):xs) = catToResults xs

nonEmptySubsequences :: [a] -> [[a]]
nonEmptySubsequences xs = filter (not . null) (subsequences xs)

-- score<num> for all sets of all partitions of [Die]
-- return _highest_ score or Nothing
scoreAll :: [Die] -> Maybe Score
scoreAll xs = maxM $ catMaybes (map ((fmap sum) . (allM . (map scoreOnly))) (partitions xs))

maxM :: Ord a => [a] -> Maybe a
maxM [] = Nothing
maxM xs = Just $ maximum xs

allM :: [Maybe a] -> Maybe [a]
allM [] = Just []
allM ((Just x):xs)
  | (isNothing $ allM xs) = Nothing
  | otherwise           = Just (x:(fromJust $ allM xs))
allM (Nothing:xs) = Nothing

scoreOnly :: [Die] -> Maybe Score
scoreOnly l = scoreOnlySorted (sort l)

scoreOnlySorted :: [Die] -> Maybe Score
scoreOnlySorted [1] = Just 100
scoreOnlySorted [5] = Just 50
scoreOnlySorted [1, 1, 1] = Just 300
scoreOnlySorted [x, y, z]
  | (x == y) && (x == z) = Just (x * 100)
scoreOnlySorted [w, x, y, z]
  | (w == x) && (w == y) && (w == z) = Just 1000
scoreOnlySorted [v, w, x, y, z]
  | (v == w) && (v == x) && (v == y) && (v == z) = Just 2000
scoreOnlySorted [u, v, w, x, y, z]
  | (u == v) && (u == w) && (u == x) && (u == y) && (u == z) = Just 3000
  | (u == v) && (u == w) && (x == y) && (x == z) = Just 2500
  | (u == v) && (w == x) && (y == z) = Just 1500
scoreOnlySorted [1, 2, 3, 4, 5, 6] = Just 1500
scoreOnlySorted _ = Nothing

-- partitions [1] = [ [[1]] ]
-- partitions [2, 1] = [ [[2], [1]], [[2, 1]] ]
-- partitions [3, 2, 1] = [ [[3], [2], [1]], [[3, 2], [1]], [[2], [3, 1]], [[3], [2, 1]], [[3, 2, 1]] ]
partitions :: [a] -> [[[a]]]
partitions [x] = [[[x]]]
partitions (x:xs) =
  let sps = partitions xs
  in concat (map (addToPartition x) sps)

addToPartition :: a -> [[a]] -> [[[a]]]
addToPartition x sp = ([x]:sp):(addToSets x sp)

-- addToSets 2 [[1]] = [[[2, 1]]]
-- addToSets 3 [[2], [1]] = [ [[3, 2], [1]], [[2], [3, 1]] ]
addToSets :: a -> [[a]] -> [[[a]]]
addToSets x [p] = [[x:p]]
addToSets x (p:ps) = concat [[((x:p):ps)], (prependAll p (addToSets x ps))]

prependAll :: a -> [[a]] -> [[a]]
prependAll x xss = [x:xs | xs <- xss]
