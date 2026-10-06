-- Please define functions smaller x y, greater x y, equal x y, smaller_equal x y, greater_equal x y, and not_equal x y. 
-- No built-in functions are allowed except for < and boolean functions.

main = do
    print(smaller 2 5)
    print(greater 2 5)
    print(smaller_equal 3 3)
    print(greater_equal 3 3)
    print(not_equal 2 2)

smaller x y = x < y

greater x y = x > y

smaller_equal x y = x >= y

greater_equal x y = x <= y

not_equal x y = x /= y