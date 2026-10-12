-- The factorial n! of a natural number n can be computed as follows.
-- fac n = fac_help n 1
--   where fac_help 0 acc = acc
--     fac_help n acc = silnia_help (n - 1) (n * acc)
-- Here the variable acc of function fac_help is called an accumulator.

-- Please define a function fib n, whose value is the n-th element of the Fibonacci sequence (give two versions of fib: one recursive and one using accumulators).

main = do
    print(fib(10)) -- 55
    print(fib_acc(10))

fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fib_acc n = fib_acc_help n 0 1
    where 
        fib_acc_help 0 a b = a
        fib_acc_help n a b = fib_acc_help (n-1) b (a+b)