{-# OPTIONS_GHC -Wall #-}

-- Fibonacci numbers
-- F0 = 0
-- F1 = 1
-- Fn = Fn-1 + Fn-2  n>=2
----------------------------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Use fib to define the infinite list of all Fibonacci numbers
-- version 0 of fibAux:
-- fibAux :: [Integer] -> [Integer]
-- fibAux [] = []
-- fibAux (x:xs) = fib x : fibAux xs
---
-- version 1 of fibAux:
--fibAux :: [Integer] -> [Integer]
--fibAux xs = map fib xs
--
-- Eta reduction 
fibAux :: [Integer] -> [Integer]
fibAux = map fib

-- fibs1 :: [Integer]
-- fibs1 = fibAux [0..] 

fibs1 :: [Integer]
fibs1 = map fib [0..]

-------------------------------------
-- Exercise 2. 
-- fibs2 :: [Integer]
-- fibs2 = foldr ((:).fib) [] [0..]

-- ///////////////////////////////////////////////////////////
-- fibs2 :: [Integer]
-- fibs2 = let fib2 a b = (a+b) : fib2 b (a+b) in 0:1:fib2 0 1
-- ///////////////////////////////////////////////////////////

-- Mathematical definition
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n-1) + fib (n-2) n>=2
          
f :: Integer -> Integer -> [Integer]
f 0 1 = [0,1,1] ++ f 1 1
f x y = (x+y) : f y (x+y)   

---------------------------
-- Exercise 3 Streams 
data Stream a = Cons a (Stream a)

-- streamToList :: Stream a -> [a]
streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

-- Show only some prefix of a stream: say, the first 20 elements
instance Show a => Show (Stream a) where 
    show  = showN 20 1  where   -- before eta reduce:: show stream = showN 20 1 stream
        showN n counter (Cons a s)
          | counter < n = show a ++ showN n (counter+1) s
          | otherwise   = show a

---------------------------
-- Exercise 4 
-- streamRepeat   :: a -> Stream a
-- streamMap      :: (a -> b) -> Stream a -> Stream b
-- streamFromSeed :: (a -> a) -> a -> Stream a

-------
-- EOF:
-------