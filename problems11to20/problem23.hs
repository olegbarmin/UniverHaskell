rnd_select :: Int -> Int -> [Int]
rnd_select a b = [a..b]

main = do
    print(rnd_select 4 9)