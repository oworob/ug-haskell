-- Please define a function foldr analogous to function foldl. What is the type of foldr?

main = do
    print(foldl2 (+) 1 [1,2,3,4])
    print(foldr2 (+) 1 [1,2,3,4])

foldl2 f e [] = e
foldl2 f e (x:xs) = foldl2 f (f e x) xs

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f e [] = e
foldr2 f e (x:xs) = f x (foldr2 f e xs)