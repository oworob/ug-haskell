-- a) Please define a function product term next a b analogous to function sum.
-- Please show, how product can be used to define both function factorial and the approximation of p using the formula
-- p/4 = 2 · 4 · 4 · 6 · 6 · 8 ··· / 3 · 3 · 5 · 5 · 7 · 7 ···

main = do
    -- print(product1 term next 3 5) -- product of element from a to b, not a*b
    -- print(factorial 5)
    -- print(pi1 1000)
    -- print(accumulate combiner 0 term next 1 4)
    -- print(sum1 4)
    -- print(filter_accumulate combiner 0 square next 2 5 is_prime)
    print(filter_accumulate combiner2 1 term next 1 10 (\i -> gcd i 10 == 1))

term a = a
next a = a+1

product1 term next a b
    | a > b = 1
    | otherwise = (term a) * (product1 term next (next a) b)

factorial a = product1 term next 1 a

pi1 n = 2 * product1 termpi nextpi 1 n where 
    termpi k = (4*k*k) / (4*k*k - 1)
    nextpi k = k + 1

-- b) Please define a function accumulate combiner null-value term next a b generalizing functions sum i product: 
-- The arguments term, next, a, and b are the same as in the definitions of sum i prod.
-- combiner is a two-argument function describing, how term a is tied together with the recursive result. null-value is the initial value used in the basic step of recursion.
-- Please also show, how sum i product can be defined using accumulate.

combiner a b = a+b

accumulate combiner null_value term next a b
    | a > b = null_value
    | otherwise = combiner (term a) (accumulate combiner null_value term next (next a) b)

sum1 i = accumulate combiner 0 term next 0 i

-- c) Even accumulate can be generalized: Please define a function filter-accumulate with an additional argument pred.
-- pred is a one-argument predicate and filter-accumulate ties term a to the recursive result only if a fulfills pred.
-- Using filter-accumulate please compute the sum of squares of the prime numbers in the interval [a,b] 
-- and the product of the natural numbers i smaller than n for which gcd(i,n) = 1.

filter_accumulate combiner null_value term next a b pred
    | a > b = null_value
    | pred(a) = combiner (term a) (filter_accumulate combiner null_value term next (next a) b pred)
    | otherwise = filter_accumulate combiner null_value term next (next a) b pred

is_prime n
    | n < 2 = False
    | otherwise = all (\d -> n `mod` d /= 0) [2 .. n-1]

square n = n*n

combiner2 a b = a*b
