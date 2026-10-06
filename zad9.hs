-- Please define the following functions for lists:

main = do
    let l1 = [1, 2, 3, 4, 5]
    let l2 = [6, 7, 8, 9, 10]
    print(append l1 l2)
    print(member 3 l1)
    print(reverse2 l1)
    print(last2 l1)
    print(delete 2 l1)
    print(pairing l1 l2)
    print(split 3 (append l1 l2))
    print(map2 square l1)

-- a) append l m concatenating l i m.

append [] l2 = l2
append (h1:t1) t2 = h1:(append t1 t2)

-- b) member x l checking whether x is an element of l

member x [] = False
member x (h:t) = x == h || member x t

-- c) reverse l reversing l.

reverse2 l = rev l [] where
    rev [] acc = acc
    rev (h:t) acc = rev t (h:acc)

-- d) last l giving the last element of l.

last2 [h] = h
last2 (h:t) = last2(t)

-- e) delete x l erasing x from l.
delete x (h:t)
    | x == h = t
    | otherwise = h:delete x t

-- f) pairing l1 l2, transforming l1 i l2 into a list of corresponding pairs. Example: pairing [1,2,3] [a,b,c] [(1,a),(2,b),(3,c)] 

pairing l1 l2 = pair l1 l2 [] where
    pair [] [] acc = reverse acc
    pair (h1:t1) (h2:t2) acc = pair t1 t2 ((h1,h2):acc)

-- g) split x l splitting l into two lists l1 i l2, where l1 contains the elements smaller than x and l2 the elements greater than x.

split x l = spl x l [] [] where
    spl x [] acc1 acc2 = (reverse acc1, reverse acc2)
    spl x (h:t) acc1 acc2
        | h < x = spl x t (h:acc1) acc2
        | h > x = spl x t acc1 (h:acc2)
        | otherwise = spl x t acc1 acc2

-- h) map f l applying the one-argument function f to all elements of l.

square a = a * a

map2 f [] = []
map2 f (h:t) = f(h) : map2 f t 