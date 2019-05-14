removeAt :: Int -> [a] -> ([a])
removeAt 1 (x:xs) = (xs)
removeAt n (x:xs) = (x:r)
    where r = removeAt (n - 1) xs

main = do
    print(removeAt 2 "abcd")