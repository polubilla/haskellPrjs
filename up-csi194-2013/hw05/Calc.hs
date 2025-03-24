{-# OPTIONS_GHC -Wall #-}
-- {-# LANGUAGE TemplateHaskell     #-}
-- {-# Language AllowAmbiguousTypes #-}

module Calc where

import ExprT 
import Parser (parseExp)

------------------------
-- Exercise 1
------------------------
{-- module ExprT where
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)
--}

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = (+) (eval e1) (eval e2)
eval (Mul e1 e2) = (*) (eval e1) (eval e2)

------------------------
-- Exercise 2
------------------------
-- Parser.hs exports parseExp: parser for arithmetic expressions
{-- If you pass the constructors of ExprT to it as arguments, 
it will convert Strings representing arithmetic expressions into values of type ExprT:
example: 
    *Calc> parseExp Lit Add Mul "(2+3)*4"
    Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

    *Calc> parseExp Lit Add Mul "2+3*4"
    Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))

    *Calc> parseExp Lit Add Mul "2+3*"
    Nothing
--}
{-- evalStr "(2+3)*4" = Just 20
    evalStr "2+3*4"   = Just 14
    evalStr "2+3*"    = Nothing
--}
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of 
               Just e -> Just (eval e)
               Nothing  -> Nothing

------------------------
-- Exercise 3
------------------------
-- {-# Language AllowAmbiguousTypes #-}
--reify :: ExprT -> ExprT
--reify = id

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where    
  lit :: Integer -> ExprT
  lit = Lit
  add :: ExprT -> ExprT -> ExprT
  add = Add
  mul :: ExprT -> ExprT -> ExprT
  mul = Mul

------------------------
-- Exercise 4
------------------------
-- 4.1 
instance Expr Integer where
  lit :: Integer -> Integer
  add :: Integer -> Integer -> Integer
  mul :: Integer -> Integer -> Integer
  lit = id
  add = (+)  
  mul = (*)

-- 4.2
instance Expr Bool where 
  lit :: Integer -> Bool
  add :: Bool -> Bool -> Bool
  mul :: Bool -> Bool -> Bool    
  lit = (>0)  -- lit i = i>0  
  add = (||)  -- add i j = i || j  
  mul = (&&)  -- mul i j = i && j

-- 4.3
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7   Integer deriving (Eq, Show)

instance Ord MinMax where
  (<=) :: MinMax -> MinMax -> Bool
  (<=) m1 m2 = m1 <= m2

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit n =  Mod7 (mod n 7)
  add (Mod7 i) (Mod7 j) = Mod7 (mod (i+j) 7)
  mul (Mod7 i) (Mod7 j) = Mod7 (mod (i*j) 7)

-- Test ex4
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7
-----------------------

------------------------
-- Exercise 5
------------------------




-------
-- EOF:
-------