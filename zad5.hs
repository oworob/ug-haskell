-- Please define functions gcd a b and lcm a b whose values are the greatest common divisor and the least common multiple of (natural numbers) a and b.

main = do
    print(gcd2 8 12)
    print(lcm2 8 12)

gcd2 a 0 = a
gcd2 a b = gcd2 b (mod a b)

lcm2 a b = a * b `div` (gcd2 a b)