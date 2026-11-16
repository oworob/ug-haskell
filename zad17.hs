-- Please define a function dc realizing the scheme for solving divide-and-conquer-problems. dc has the following (functional) arguments:
-- test testing whether the problem instance is easy enough
-- end solving easy problem instances
-- divide dividing a problem into subproblems
-- combine combining solutions of subproblems.
-- Please define mergesort and Karatsuba-multiplication using dc.

main = do
    print(mergesort (<=) [4,2,5,3,1])
    print(karatsuba 100 100)

dc test end divide combine p
    | test p = end p
    | otherwise = combine p (map (dc test end divide combine) (divide p))

mergesort compare = dc test end divide combine where
    test l = length l <= 1
    end l = l
    divide l = let (left, right) = splitAt (length l `div` 2) l in [left, right]
    combine _ [left, right] = merge compare left right

merge compare [] l2 = l2
merge compare l1 [] = l1
merge compare (h1:t1) (h2:t2)
    | compare h1 h2 = h1 : merge compare t1 (h2:t2)
    | otherwise = h2 : merge compare (h1:t1) t2

karatsuba a b = dc test end divide combine (a, b) where
    test (a,b) = a < 10 || b < 10
    end (a,b) = a*b
    divide (a,b) =
        let m = max (digits a) (digits b) `div` 2
            base = 10^m
            (ah, al) = a `divMod` base
            (bh, bl) = b `divMod` base
            in [(ah,bh), (al,bl), (ah+al,bh+bl)]

    combine (a,b) [z2,z0,zs] =
        let m = max (digits a) (digits b) `div` 2
            base = 10^m
        in z2 * base * base + (zs-z2-z0) * base + z0

digits n = length (show n)