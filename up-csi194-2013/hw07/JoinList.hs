----------------------------
-- HW07 - 
----------------------------
-- TypeSynonymInstances
{-# LANGUAGE FlexibleInstances  #-}

import Data.Monoid ((<>))

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-------------------------------------------------------------------------
-- Exercise 1
-- Write an append function for JoinLists that yields a new JoinList 
-- whose monoidal annotation is derived from those of the two arguments:
-------------------------------------------------------------------------
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append m l1 l2
            where m = mappend (tag l1) (tag l2)

-- Or using the synonym (<>) for mappend: (+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2 

-- helper function tag: gets the annotation at the root of a JoinList.
tag :: Monoid m => JoinList m a -> m
tag Empty            = mempty
tag (Single m a)     = m
tag (Append m l1 l2) = m

-------------------------------------------------------------------------
-- Exercise 2
-- 
-------------------------------------------------------------------------
-------  
-- EOF:
-------
