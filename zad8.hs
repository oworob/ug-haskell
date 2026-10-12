-- The exponentiation b^e (for natural numbers b and e) can be computed using b^e = (b^(e/2))^2.
-- Please write an exponentiation function exp b e using this property (again one version using recursion and one using accumulators).

main = do
    print(exp1 2 5)
    print(exp2 2 5)

exp1 b 1 = b
exp1 b 0 = 1
exp1 b e = 
    let x = (exp1 b (e `div` 2)) in
        if even e then x*x else x*x*b

exp2 b e = expacc b e 1 where
    expacc b 0 acc = acc
    expacc b e acc
        | even e = expacc (b*b) (e `div` 2) acc
        | odd e = expacc b (e-1) (acc*b)