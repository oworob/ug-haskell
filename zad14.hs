-- Using function foldl or foldr please define the following functions

l = [1,2,3,4]
l2 = [True, True, False, True]
l3 = [12,8,20]

main = do
    print(prod l)
    print(length1 l)
    print(and1 l2)
    print(gcd1 l3)
    print(reverse1 l)
    print(delete 2 l)
    print(map1 (\n -> n*n) l)
    print(filter1 l even)
    print(forall1 l3 even)

-- a) prod (returning the product of the elements of a list)

prod l = foldl (*) 1 l

-- b) length

length1 l = foldl (\a b -> a + 1) 0 l -- \ defines anonymous function

-- c) and (for a list of Boolean values)

and1 l = foldl (\a b -> a && b) True l

-- d) gcd (for a list of natural numbers)

gcd1 (x:xs) = foldl gcd x xs

-- e) reverse

reverse1 l = foldl (\acc x -> x : acc) [] l

-- f) delete

delete x l = reverse(fst (foldl (\(acc, deleted) y -> if y == x && deleted == False then (acc,True) else (y:acc,False)) ([], False) l))

-- g) map

map1 f l = reverse(foldl (\acc x -> (f x) : acc) [] l)

-- h) filter pred

filter1 l pred = reverse(foldl (\acc x -> if (pred x) then x:acc else acc) [] l)

-- i) forall pred.

forall1 l pred = foldl (\acc x -> if (pred x) && acc == True then True else False) True l