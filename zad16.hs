-- Please define a function iter f n for a one-argument function f and a natural number n. The value of iter f n is a function computing f^n.
-- Examples:
-- (iter square 2) 3 -> 81 (3^2^2)
-- (iter (\x -> x + 3) 2) 5 -> 11 (5+3+3)
-- (iter square 0) 7 -> 7 (7)

main = do
    print((iter (\x -> x*2) 3) 2)

iter f n
    | n == 0 = (\x -> x)
    | n == 1 = f
    | otherwise = (\x -> iter f (n-1) (f x))