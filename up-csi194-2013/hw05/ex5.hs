{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE TypeSynonymInstances #-}

--module Ex5  where

import Parser (parseExp)
import qualified StackVM as S

-- 5.1 : create an instance of the Expr type class for Program, 
--       so that arithmetic expressions can be interpreted as compiled programs.
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr S.Program where
   lit :: Integer-> S.Program
   lit n = [S.PushI n]
   add :: S.Program -> S.Program -> S.Program
   add p1 p2 = p1 ++ p2 ++ [S.Add]
   mul :: S.Program -> S.Program -> S.Program
   mul p1 p2 = p1 ++ p2 ++ [S.Mul]

-- Your task is to implement a compiler for arithmetic expressions
-- put together the pieces you have to create a function:
-- takes Strings representing arithmetic expressions and 
-- compiles them into programs that can be run on the custom CPU.
compile :: String -> Maybe S.Program
compile = parseExp lit add mul


-------
-- EOF:
-------