{-# OPTIONS_GHC -Wall #-}
-------------
-- Exercise 1
-------------
-- Option 0
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
   | even x    = (x - 2) * fun1 xs
   | otherwise = fun1 xs

-- Option 1
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x-2) . filter even

------------------------
-- Option 0
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
   | even n    = n + fun2 (n `div` 2)
   | otherwise =     fun2 (3*n + 1)

-- Option 1
fun2' :: Integer -> Integer
fun2' = sum . takeWhile (/=1) . iterate (\x -> if even x then div x 2 else 3*x + 1 )

-----------------------------------------
-- Exercise 2
type Height = Integer

data Tree a = Leaf
            | Node Height (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h                      --- height (Node h left x right) = h              

-- Revisar insercion !!!
insertBalanced :: a -> Tree a -> Tree a
insertBalanced x Leaf = Node 0 Leaf x Leaf
insertBalanced x (Node h left y right)
   | height left - height right <= 0  = Node (h+1) (insertBalanced x left) y right
   | otherwise                        = Node (h+1) left y (insertBalanced x right)

foldTree :: [a] -> Tree a
-- version 0:
--foldTree []     = Leaf
--foldTree (x:xs) = insertBalanced x (foldTree xs)

-- version 1:
-- El compilador sugiere el siguiente primer cambio: 
-- foldTree xs = foldr insertBalanced Leaf xs

-- version 2: 
-- Luego sugerie una eta-reduction para tener un pointfree style : eliminar la variable xs (el "punto")
foldTree = foldr insertBalanced Leaf

---------------------------
-- Exercise 3
-- xor :: Eq a => a -> a -> Bool
-- xor a b = a /= b

-- myxor :: [Bool] -> Bool
-- myxor = foldr xor False
-------
xor :: [Bool] -> Bool
xor = foldr (/=) False



-------
-- EOF:
-------