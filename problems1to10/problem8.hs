compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs) --- dropWhile берет по одному елементу из данного списка до тех пор пока выполняется данное условие


main = do
    print (compress "aaaabbbcccddccc")