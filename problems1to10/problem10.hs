encodeImpl :: (Eq a) => [a] -> Int -> [(Int, a)]
encodeImpl [a] b = [(b+1,a)] 
encodeImpl (x:xs) b = if x == head xs then encodeImpl xs (b+1)  else [(b+1,x)] ++ encodeImpl xs 0 

encode :: (Eq a) => [a] -> [(Int, a)] 
encode xs = encodeImpl xs 0

main = do
    print(encode "aaaabc")