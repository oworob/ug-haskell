-- a) Please define a function map2 f l1 l2 applying the one-argument function f to all elements of l1 i l2. Example:
-- map2 (+) [1,2,3] [8,9,10] -> [9,11,13]

main = do
    let l1 = [1, 2, 3, 4, 5]
    let l2 = [6, 7, 8, 9, 10]
    -- print(map2 (+) l1 l2)
    -- print(filter2 even l1)
    -- print(take_while odd [1,3,5,6,7])
    print(compress [1,1,3,3,3,3,2,2,2,2,2,7,7,2,2])

map2 f [] [] = []
map2 f (h1:t1) (h2:t2) = (f h1 h2) : map2 f t1 t2

-- b) Please define a function filter p l, whose value is the list of all elements of l fulfilling the one-argument predicate p.

filter2 p l = filt p l [] where
    filt p [] acc = reverse acc
    filt p (h:t) acc
        | p h = filt p t (h:acc)
        | otherwise = filt p t acc

-- c) Please define a function take_while p l that returns a list of elements of list l (starting from the left) until the first failure of predicate p. Examples:
-- take_while odd [1,3,5,6,7] -> [1,3,5]
-- take_while (\x -> x > 0) [1,4,2,-1,5] -> [1,4,2] 

take_while p [] = []
take_while p (h:t)
    | p h = h : take_while p t
    | otherwise = []


-- d) Please define a function groups l that will append consecutive identical elements from list l. Example:
-- compress [1,1,3,3,3,3,2,2,2,2,2,7,7,2,2] -> [1,1],[3,3,3,3],[2,2,2,2,2],[7,7],[2,2]]

compress (h:t) = comp t [h] [] where -- acc1 = small, acc2 = big
    comp [] acc1 acc2 = reverse (acc1 : acc2)
    comp (h:t) (ha:ta) acc2
        | h == ha = comp t (h : ha : ta) acc2
        | otherwise = comp t [h] ((ha : ta) : acc2)
