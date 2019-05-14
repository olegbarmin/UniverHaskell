dropEvery :: [a] -> Int -> [a]
dropEvery xs n = drop xs n
    where drop [] _ = []
          drop (x:xs) 1 = drop xs n
          drop (x:xs) k = x : drop xs (k-1)


main = do
    print(dropEvery "abcabc" 3)