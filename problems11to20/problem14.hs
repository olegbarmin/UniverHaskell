dublicateOwn :: Eq a => [a] -> [a]
dublicateOwn [] = []
dublicateOwn (x: xs) = x:x: dublicateOwn xs

main = do
    print(dublicateOwn "abc")