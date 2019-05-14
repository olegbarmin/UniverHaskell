insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1)  -- "take" - sublist from the begining to index, "drop" sublislt from the index to the end

main = do
    print(insertAt 'X' "abcd" 2)