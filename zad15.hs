-- a) Please define function insertionsort :: (a -> a -> Bool) -> [a] -> [a] (preferably using fold).

main = do
    print (insertionsort (<=) [4,2,5,3,1])
    print (quicksort (<=) [4,2,5,3,1])

insertionsort :: (a -> a -> Bool) -> [a] -> [a]
insertionsort compare l = foldl step [] l where
    step [] x = [x]
    step sorted x = insert compare x sorted

insert compare x [] = [x]
insert compare x (h:t)
    | compare x h = x:h:t
    | otherwise = h : insert compare x t

-- b) Please define function quicksort :: (a -> a -> Bool) -> [a] -> [a].
-- (Before defining quicksort it might be useful to have a look at Haskell's list comprehension syntax.)

quicksort :: (a -> a -> Bool) -> [a] -> [a]
quicksort compare [] = []
quicksort compare (h:t) =
    let smaller = [x | x <- t, compare x h]
        bigger = [x | x <- t, not(compare x h)]
    in quicksort compare smaller ++ [h] ++ quicksort compare bigger