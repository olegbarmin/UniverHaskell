replicateOwn :: [a] -> Int -> [a]
replicateOwn xs n = concatMap (replicate n) xs

main = do
    print(replicateOwn "abc" 3)