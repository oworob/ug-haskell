-- Please define functions odd n and even n checking whether a natural number n is odd or even, resp. 
-- No built-in functions are allowed except for True, False, ==, and a function "-1" calculating n-1 for natural numbers n.

main = do
    print(even2 5)
    print(odd2 5)


odd2 0 = False
odd2 n = even2(n-1)

even2 0 = True
even2 n = odd2(n-1)