-- Please define a function same_values p1 p2 x y whose value is True, if p1(x,y) and p2(x,y) have the same value and False otherwise.

main = do
    print (same_values (+) (*) 2 2)

same_values p1 p2 x y = p1 x y == p2 x y