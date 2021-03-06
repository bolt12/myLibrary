module MyPrelude
    where

import Data.Char (isDigit)
import Prelude hiding (
                      enumFromTo, 
                      enumFromThenTo, 
                      concat, 
                      last, 
                      init, 
                      (!!), 
                      reverse, 
                      take, 
                      drop, 
                      zip,
                      elem,
                      replicate,
                      intersperce,
                      group,
                      inits,
                      tails,
                      isPrefixOf,
                      isSuffixOf,
                      isSubsequenceOf,
                      nub,
                      delete,
                      (\\),
                      union,
                      intersect,
                      insert,
                      maximum,
                      minimum,
                      sum,
                      product,
                      and,
                      or,
                      unwords,
                      unlines,
                      )

enumFromTo :: Int -> Int -> [Int]
enumFromTo x y = [x..y]

enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo x y z = [x, y .. z]

concat :: [a] -> [a] -> [a]
concat [] [] = []
concat xs l = foldr (:) l xs

last :: [a] -> a
last [x] = x
last (x:xs) = last xs

init :: [a] -> [a]
init [] = []
init [x] = []
init (x:xs) = x : init xs

(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n-1)

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 l = l
drop _ [] = []
drop n (x:xs) = drop (n-1) xs

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

elem :: Eq a => a -> [a] -> Bool
elem e [] = False
elem e (x:xs) = if e == x then True else elem e xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a : replicate (n-1) a

intersperce :: a -> [a] -> [a]
intersperce a [] = []
intersperce a (x:xs) = x : a : intersperce a xs

group :: Eq a => [a] -> [[a]]
group (x:xs) = go [x] xs
    where
        go l [] = [l]
        go l@(c:cs) (h:t) = if c == h then go (h:l) t
                                      else l : go [h] t

inits :: [a] -> [[a]]
inits (x:xs) = [] : go [x] xs
    where
        go l [] = [l]
        go l (h:t) = l : go (l ++ [h]) t

tails :: [a] -> [[a]]
tails [] = [[]]
tails l@(x:xs) = l : tails xs

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = if x == y then isPrefixOf xs ys else False

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf l [] = False
isSuffixOf l l2 = if l == l2 then True else isSuffixOf l (tail l2)

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) = if x == y then isSubsequenceOf xs ys else isSubsequenceOf (x:xs) ys

elemIndexes :: Eq a => a -> [a] -> [Int]
elemIndexes x l = go 0 x l
    where
        go _ x [] = []
        go i x (h:t) = if x == h then i : go (i+1) x t else go (i+1) x t

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (h:t) = if x == h then t else h : delete x t

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l [] = l
(\\) l (x:xs) = (delete x l) \\ xs

union :: Eq a => [a] -> [a] -> [a]
union l1 l2 = l1 ++ union' l2 l1
    where
        union' [] _ = []
        union' (x:xs) l | x `elem` l = union' xs l
                        | otherwise = x : union' xs (x:l)
                    
intersect :: Eq a => [a] -> [a] -> [a]
intersect l1 l2 = filter ((flip elem) l2) l1

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) | x > h = h : insert x t
               | otherwise = x : h : t

maximum :: Ord a => [a] -> a
maximum = foldr1 max

minimum :: Ord a => [a] -> a
minimum = foldr1 min

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

unwords :: [String] -> String
unwords [] = ""
unwords l  = foldr1 addSpace l
    where
        addSpace x y = x ++ " " ++ y

unlines :: [String] -> String
unlines [] = ""
unlines l = foldr1 addLine l
    where
        addLine x y = x ++ "\n" ++ y

maxIndex :: Ord a => [a] -> Int
maxIndex (x:xs) = go 0 0 x xs
    where
        go _ m _ [] = m
        go n m e (h:t) | e > h = go (n+1) m e t
                       | otherwise = go (n+1) (n+1) h t

hasRepeated :: Eq a => [a] -> Bool
hasRepeated [] = False
hasRepeated (x:xs) | any (==x) xs = True
                   | otherwise = hasRepeated xs

digits :: String -> String
digits = filter (isDigit)

oddIndexes :: [a] -> [a]
oddIndexes (x:xs) = go 1 xs
    where
        go _ [] = []
        go i (h:t) | i `mod` 2 /= 0 = h : go (i+1) t
                   | otherwise = go (i+1) t

evenIndexes :: [a] -> [a]
evenIndexes l = go 0 l
    where
        go _ [] = []
        go i (h:t) | i `mod` 2 == 0 = h : go (i+1) t
                   | otherwise = go (i+1) t

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) | x < y = isSorted (y:xs)
                  | otherwise = False

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x $ iSort xs

elemMSet :: Eq a => a -> [(a, Int)] -> Bool
elemMSet x [] = False
elemMSet x ((a,b):xs) | x == a = True
                      | otherwise = elemMSet x xs

lengthMSet :: [(a, Int)] -> Int
lengthMSet = foldr ((+) . snd) 0

convertMSet :: [(a, Int)] -> [a]
convertMSet = concatMap (uncurry (flip replicate))

insertMSet :: Eq a => a -> [(a, Int)] -> [(a, Int)]
insertMSet x [] = [(x, 1)]
insertMSet x ((a,b):xs) | x == a = (a,b+1):xs
                        | otherwise = (a,b):insertMSet x xs

removeMSet :: Eq a => a -> [(a, Int)] -> [(a, Int)]
removeMSet x [] = []
removeMSet x ((a,b):xs) | x == a && b > 1 = (a,b-1):xs
                        | x == a && b <= 1 = xs
                        | otherwise = (a,b):removeMSet x xs

makeMSet :: Ord a => [a] -> [(a, Int)]
makeMSet = map ( (,) <$> head <*> length ) . group 
-- (,) <$> head <*> length <==> 
-- (,) . head <*> length <==> 
-- \x -> ((,) . head) x (g x) <==> 
-- \x -> (,) (head x) (length x)
