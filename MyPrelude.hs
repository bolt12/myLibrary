module MyPrelude
    where

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
                      elemIndices,
                      nub,
                      delete,
                      (\\)
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

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = go 0 x l
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
