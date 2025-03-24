------------------------
-- Exercise 3.1 Hopscotch
------------------------

-- cond : Testing if the first parameter is divisible by d
-- cond :: Int -> Int -> Bool
-- cond _ 0 = False
-- cond 0 _ = True
-- cond m d = mod m d == 0

-- consaux:
-- n  : Index of the elements to select "every nth.."
-- xs list of tuples : (t,Int)
consaux :: Int -> [(t, Int)] -> [t]
consaux n xs  
    | n <= length xs = [a | (a,b) <- xs, mod b n == 0]   -- cond b n
    | otherwise      = []

auxSkip :: Int -> [(t, Int)] -> [[t]]
auxSkip 0 [] = []
auxSkip 0 zs = []
auxSkip n zs = auxSkip (n-1) zs ++ [consaux n zs]
             
-- skips : The output is a list of lists. 
-- The first list should be the same as the input list, thes second contains
-- every second, ... the nth contains every nth element of the list.
skips :: [t] -> [[t]]
skips [] = []
skips xs = auxSkip n zs
    where 
        n   = length xs
        zs  = zip xs idx
        idx = [1..n]


-------------
-- EOF:
-------------