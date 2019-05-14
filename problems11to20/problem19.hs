rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate x 0 = x
rotate x y
  | y > 0 = rotate (tail x ++ [head x]) (y-1) -- move left (all except first + head)
  | otherwise = rotate (last x : init x) (y+1) -- move right(last + all except last)


main = do
    print(rotate ['a','b','c','d','e','f','g','h'] 3)