-- Please define a function roots a b c, whose values are the roots of the quadratic polynomial ax^2 + bx + c.

main = do
    print(roots 1 2 3)
    print(roots 1 2 1)
    print(roots 1 (-5) 6)

roots a b c =
    let delta = b * b - 4 * a * c in 
        if delta < 0 then (0,0)
        else let
            x1 = (-b + sqrt delta) / (2 * a)
            x2 = (-b - sqrt delta) / (2 * a)
        in (x1, x2)