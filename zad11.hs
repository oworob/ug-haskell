-- What are the (polymorphic) types of the functions defined in exercises 4,5,6,9 and 10?

-- 4
same_values :: Eq b => (a -> a -> b) -> (a -> a -> b) -> a -> a -> Bool
same_values p1 p2 x y = p1 x y == p2 x y

-- 5
gcd2 :: Integral a => a -> a -> a
gcd2 a 0 = a
gcd2 a b = gcd2 b (mod a b)

lcm2 :: Integral a => a -> a -> a
lcm2 a b = a * b `div` (gcd2 a b)

-- 6
odd2 :: Integral a => a -> Bool
odd2 0 = False
odd2 n = even2(n-1)

even2 :: Integral a => a -> Bool
even2 0 = True
even2 n = odd2(n-1)

-- 9

append :: [a] -> [a] -> [a]
append [] l2 = l2
append (h1:t1) t2 = h1:(append t1 t2)

member :: Eq a => a -> [a] -> Bool
member x [] = False
member x (h:t) = x == h || member x t

reverse2 :: [a] -> [a]
reverse2 l = rev l [] where
    rev [] acc = acc
    rev (h:t) acc = rev t (h:acc)

last2 :: [a] -> a
last2 [h] = h
last2 (h:t) = last2(t)

delete :: Eq a => a -> [a] -> [a]
delete x (h:t)
    | x == h = t
    | otherwise = h:delete x t

pairing :: [a] -> [b] -> [(a,b)]
pairing l1 l2 = pair l1 l2 [] where
    pair [] [] acc = reverse acc
    pair (h1:t1) (h2:t2) acc = pair t1 t2 ((h1,h2):acc)

split :: Integral a => a -> [a] -> ([a],[a])
split x l = spl x l [] [] where
    spl x [] acc1 acc2 = (reverse acc1, reverse acc2)
    spl x (h:t) acc1 acc2
        | h < x = spl x t (h:acc1) acc2
        | h > x = spl x t acc1 (h:acc2)
        | otherwise = spl x t acc1 acc2

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (h:t) = f(h) : map2 f t

-- 10

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f [] [] = []
map2 f (h1:t1) (h2:t2) = (f h1 h2) : map2 f t1 t2

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p l = filt p l [] where
    filt p [] acc = reverse acc
    filt p (h:t) acc
        | p h = filt p t (h:acc)
        | otherwise = filt p t acc
    
take_while :: (a -> Bool) -> [a] -> [a]
take_while p [] = []
take_while p (h:t)
    | p h = h : take_while p t
    | otherwise = []

compress :: Eq a => [a] -> [[a]]
compress (h:t) = comp t [h] [] where -- acc1 = small, acc2 = big
    comp [] acc1 acc2 = reverse (acc1 : acc2)
    comp (h:t) (ha:ta) acc2
        | h == ha = comp t (h : ha : ta) acc2
        | otherwise = comp t [h] ((ha : ta) : acc2)